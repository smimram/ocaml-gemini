module Client = struct
  open Cohttp
  open Lwt.Infix

  let default_url = "https://generativelanguage.googleapis.com"

  type role =
    | User
    | Model
    | System

  let string_of_role = function
    | User -> "user"
    | Model -> "model"
    | System -> "user"


  type message = {
    role : role;
    parts : string list;
  }

  let json_of_message { role; parts } =
    let json_of_part text = `Assoc ["text", `String text] in
    `Assoc
      [
        "role", `String (string_of_role role);
        "parts", `List (List.map json_of_part parts)
      ]

  type request = {
    model : string;
    messages : message list;
  }

  module Answer = struct
    let text json =
      let open Yojson.Safe.Util in
      json
      |> member "candidates" |> to_list |> List.hd
      |> member "content" |> member "parts" |> to_list |> List.hd |> member "text"
      |> to_string

    let error_message json =
      let open Yojson.Safe.Util in
      try Some (json |> member "error" |> member "message" |> to_string) with
      | _ -> None
  end

  let generate_content ?(url=default_url) ~key ~model ~messages () =
    if messages = [] then Error "generate_content requires at least one message"
    else
    let path = Printf.sprintf "/v1beta/models/%s:generateContent" (Uri.pct_encode model) in
    let uri =
      Uri.of_string url
      |> fun u -> Uri.with_path u path
      |> fun u -> Uri.add_query_param' u ("key", key)
    in
    let body = Yojson.Safe.to_string @@ `Assoc ["contents", `List (List.map json_of_message messages)]  in
    let headers = Header.init_with "content-type" "application/json" in
    let result =
      let open Lwt.Syntax in
      Lwt_main.run
        (
          let* (resp, body_stream) = Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri in
          Cohttp_lwt.Body.to_string body_stream >|= fun body_text ->
          let status = Response.status resp in
          match Yojson.Safe.from_string body_text with
          | exception _ -> Error (Printf.sprintf "Gemini API returned non-JSON response (%d): %s" (Code.code_of_status status) body_text)
          | json ->
            if Code.is_success (Code.code_of_status status) then
              (
                try Ok (Answer.text json)
                with _ -> Error ("Gemini API success response could not be parsed: " ^ body_text)
              )
            else
              let message =
                match Answer.error_message json with
                | Some m -> m
                | None -> body_text
              in
              Error (Printf.sprintf "Gemini API error (%d): %s" (Code.code_of_status status) message)
        )
    in
    result
end
