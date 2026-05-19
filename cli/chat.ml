module C = Gemini.Client

let default_model = "gemini-1.5-flash"

let print_help () =
  print_endline "Commands:";
  print_endline "  /help           Show this help";
  print_endline "  /quit or /exit  Leave the chat"

let rec loop client model history =
  print_string "you> ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
    print_endline "";
    print_endline "Bye."
  | input ->
    let line = String.trim input in
    if line = "" then loop client model history
    else if line = "/help" then (
      print_help ();
      loop client model history
    )
    else if line = "/quit" || line = "/exit" then
      print_endline "Bye."
    else
      let user_message = { C.role = User; parts = [line] } in
      let request = C.make_request ~model (history @ [ user_message ]) in
      match C.generate_content client request with
      | Error msg ->
        prerr_endline ("Gemini error: " ^ msg);
        loop client model history
      | Ok response ->
        Printf.printf "gemini> %s\n%!" response.text;
        let model_message = { C.role = Model; parts = [response.text] } in
        loop client model (history @ [ user_message; model_message ])

let () =
  let model = ref default_model in
  let api_key = ref None in
  let url = ref None in
  let anon_args = ref [] in
  let set_api_key s = api_key := Some s in
  let set_base_url s = url := Some s in
  let usage = "ocaml-gemini-chat [--model MODEL] [--api-key KEY] [--base-url URL]" in
  let specs =
    Arg.align
      [
        "--model", Arg.Set_string model, "Model to use";
        "--api-key", Arg.String set_api_key, "API key";
        "--url", Arg.String set_base_url, "Base URL"
      ]
  in
  Arg.parse specs (fun s -> anon_args := s :: !anon_args) usage;
  if !anon_args <> [] then
    (
      prerr_endline "Unexpected positional arguments.";
      Arg.usage specs usage;
      exit 2
    );
  print_endline ("Model: " ^ !model);
  print_endline "Type /help for commands.";
  let client = C.create ?url:!url ?api_key:!api_key () in
  loop client !model []
