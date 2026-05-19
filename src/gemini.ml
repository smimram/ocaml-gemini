module Client = struct
  open Cohttp
  open Lwt.Infix

  type t = {
    base_url : string;
    api_key : string option;
  }

  type role =
    | User
    | Model
    | System

  type part =
    | Text of string

  type message = {
    role : role;
    parts : part list;
  }

  type generation_config = {
    temperature : float option;
    top_p : float option;
    top_k : int option;
    max_output_tokens : int option;
  }

  type request = {
    model : string;
    messages : message list;
    generation_config : generation_config option;
  }

  type response = {
    text : string;
    raw : Yojson.Safe.t;
  }

  let default_base_url = "https://generativelanguage.googleapis.com"

  let create ?(base_url = default_base_url) ?api_key () = { base_url; api_key }

  let make_request ?generation_config ~model messages =
    { model; messages; generation_config }

  let role_to_string = function
    | User -> "user"
    | Model -> "model"
    | System -> "user"

  let part_to_yojson = function
    | Text text -> `Assoc [ ("text", `String text) ]

  let message_to_yojson { role; parts } =
    `Assoc
      [ ("role", `String (role_to_string role));
        ("parts", `List (List.map part_to_yojson parts))
      ]

  let generation_config_to_yojson cfg =
    let fields =
      []
      |> (fun acc ->
          match cfg.temperature with
          | None -> acc
          | Some v -> ("temperature", `Float v) :: acc)
      |> (fun acc ->
          match cfg.top_p with
          | None -> acc
          | Some v -> ("topP", `Float v) :: acc)
      |> (fun acc ->
          match cfg.top_k with
          | None -> acc
          | Some v -> ("topK", `Int v) :: acc)
      |> (fun acc ->
          match cfg.max_output_tokens with
          | None -> acc
          | Some v -> ("maxOutputTokens", `Int v) :: acc)
    in
    `Assoc (List.rev fields)

  let json_of_request request =
    let base = [ ("contents", `List (List.map message_to_yojson request.messages)) ] in
    let fields =
      match request.generation_config with
      | None -> base
      | Some cfg -> ("generationConfig", generation_config_to_yojson cfg) :: base
    in
    `Assoc fields

  let trim_trailing_slash s =
    if String.length s > 0 && s.[String.length s - 1] = '/' then
      String.sub s 0 (String.length s - 1)
    else
      s

  let extract_text_from_response json =
    let open Yojson.Safe.Util in
    json |> member "candidates" |> to_list |> List.hd
    |> member "content" |> member "parts" |> to_list |> List.hd |> member "text"
    |> to_string

  let extract_error_message json =
    let open Yojson.Safe.Util in
    try Some (json |> member "error" |> member "message" |> to_string) with
    | _ -> None

  let generate_content client request =
    let api_key =
      match client.api_key with
      | Some key -> Some key
      | None -> Sys.getenv_opt "GEMINI_API_KEY"
    in
    match api_key with
    | None -> Error "Missing Gemini API key. Pass ~api_key or set GEMINI_API_KEY"
    | Some key ->
      let base_url = trim_trailing_slash client.base_url in
      let path =
        Printf.sprintf "/v1beta/models/%s:generateContent" (Uri.pct_encode request.model)
      in
      let uri =
        Uri.of_string base_url
        |> fun u -> Uri.with_path u path
        |> fun u -> Uri.add_query_param' u ("key", key)
      in
      let body = json_of_request request |> Yojson.Safe.to_string in
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
                (try Ok { text = extract_text_from_response json; raw = json } with
                 | _ -> Error ("Gemini API success response could not be parsed: " ^ body_text))
              else
                let message =
                  match extract_error_message json with
                  | Some m -> m
                  | None -> body_text
                in
                Error (Printf.sprintf "Gemini API error (%d): %s" (Code.code_of_status status) message)
          )
      in
      result
end
