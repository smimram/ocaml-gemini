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

  module Request = struct
    type t = {
      model : string;
      messages : message list;
    }

    let uri ~url ~key ~model ?(stream=false) () =
      let method_name = if stream then "streamGenerateContent" else "generateContent" in
      let path = Printf.sprintf "/v1beta/models/%s:%s" (Uri.pct_encode model) method_name in
      let base_uri =
        Uri.of_string url
        |> fun uri -> Uri.with_path uri path
        |> fun uri -> Uri.add_query_param' uri ("key", key)
      in
      if stream then Uri.add_query_param' base_uri ("alt", "sse") else base_uri

    let body ?(tools=[]) messages =
      let json_of_part text = `Assoc ["text", `String text] in
      let json_of_message { role; parts } =
        `Assoc
          [
            "role", `String (string_of_role role);
            "parts", `List (List.map json_of_part parts)
          ]
      in
      let contents = ["contents", `List (List.map json_of_message messages)] in
      let tools = if tools = [] then [] else [] in
      Yojson.Safe.to_string @@ `Assoc (contents@tools)
  end

  module Answer = struct
    let text json =
      let open Yojson.Safe.Util in
      json
      |> member "candidates" |> to_list |> List.hd
      |> member "content" |> member "parts" |> to_list
      |> List.filter_map (fun part ->
          try Some (part |> member "text" |> to_string) with
          | _ -> None)
      |> String.concat ""

    let error_message json =
      let open Yojson.Safe.Util in
      try Some (json |> member "error" |> member "message" |> to_string) with
      | _ -> None
  end

  let read_error_response ~status body_text =
    match Yojson.Safe.from_string body_text with
    | exception _ -> Error (Printf.sprintf "Gemini API returned non-JSON response (%d): %s" (Code.code_of_status status) body_text)
    | json ->
      let message =
        match Answer.error_message json with
        | Some msg -> msg
        | None -> body_text
      in
      Error (Printf.sprintf "Gemini API error (%d): %s" (Code.code_of_status status) message)

  let generate_content ?(url=default_url) ~key ~model ~messages () =
    assert (messages <> []);
    let uri = Request.uri ~url ~key ~model () in
    let body = Request.body messages in
    let headers = Header.init_with "content-type" "application/json" in
    let result =
      let open Lwt.Syntax in
      Lwt_main.run
        (
          let* (resp, body_stream) = Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri in
          Cohttp_lwt.Body.to_string body_stream >|= fun body_text ->
          let status = Response.status resp in
          if Code.is_success (Code.code_of_status status) then
            (
              match Yojson.Safe.from_string body_text with
              | exception _ -> Error ("Gemini API success response could not be parsed: " ^ body_text)
              | json ->
                (
                  try Ok (Answer.text json)
                  with _ -> Error ("Gemini API success response could not be parsed: " ^ body_text)
                )
            )
          else
            read_error_response ~status body_text
        )
    in
    result

  let stream_sse_response body_stream ~on_chunk =
    let body_chunks = Cohttp_lwt.Body.to_stream body_stream in
    let pending = Buffer.create 256 in
    let event_data = ref [] in
    let collected = Buffer.create 256 in
    let dispatch_event () =
      match List.rev !event_data with
      | [] -> Ok ()
      | lines ->
        event_data := [];
        let payload = String.concat "\n" lines in
        if payload = "[DONE]" then Ok ()
        else
          match Yojson.Safe.from_string payload with
          | exception _ -> Error ("Gemini API stream chunk could not be parsed: " ^ payload)
          | json ->
            let text =
              try Answer.text json
              with _ -> ""
            in
            if text <> "" then (
              on_chunk text;
              Buffer.add_string collected text
            );
            Ok ()
    in
    let handle_line line =
      let line_length = String.length line in
      let line =
        if line_length > 0 && line.[line_length - 1] = '\r' then String.sub line 0 (line_length - 1)
        else line
      in
      if line = "" then dispatch_event ()
      else if String.length line >= 5 && String.sub line 0 5 = "data:" then (
        let value = String.trim (String.sub line 5 (String.length line - 5)) in
        event_data := value :: !event_data;
        Ok ()
      )
      else Ok ()
    in
    let rec consume_pending () =
      let contents = Buffer.contents pending in
      match String.index_opt contents '\n' with
      | None -> Ok ()
      | Some index ->
        let line = String.sub contents 0 index in
        let rest = String.sub contents (index + 1) (String.length contents - index - 1) in
        Buffer.clear pending;
        Buffer.add_string pending rest;
        (
          match handle_line line with
          | Error _ as err -> err
          | Ok () -> consume_pending ()
        )
    in
    let rec loop () =
      Lwt_stream.get body_chunks >>= function
      | None ->
        if Buffer.length pending > 0 then (
          let line = Buffer.contents pending in
          Buffer.clear pending;
          match handle_line line with
          | Error _ as err -> Lwt.return err
          | Ok () -> Lwt.return (dispatch_event ())
        ) else
          Lwt.return (dispatch_event ())
      | Some chunk ->
        Buffer.add_string pending chunk;
        (
          match consume_pending () with
          | Error _ as err -> Lwt.return err
          | Ok () -> loop ()
        )
    in
    loop () >|= function
    | Error _ as err -> err
    | Ok () -> Ok (Buffer.contents collected)

  let generate_content_stream ?(url=default_url) ~key ~model ~messages ~on_chunk () =
    assert (messages <> []);
    let uri = Request.uri ~stream:true ~url ~key ~model () in
    let body = Request.body messages in
    let headers = Header.init_with "content-type" "application/json" in
    Lwt_main.run
      (
        let open Lwt.Syntax in
        let* (resp, body_stream) = Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri in
        let status = Response.status resp in
        if Code.is_success (Code.code_of_status status) then
          stream_sse_response body_stream ~on_chunk
        else
          let* body_text = Cohttp_lwt.Body.to_string body_stream in
          Lwt.return (read_error_response ~status body_text)
      )
end
