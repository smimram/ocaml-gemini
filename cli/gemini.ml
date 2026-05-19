module C = Gemini.Client

let default_model = "gemini-3.5-flash"

let print_help () =
  print_endline "Commands:";
  print_endline "  /help           Show this help";
  print_endline "  /quit or /exit  Leave the chat"

let loop ?url ~key ~model history =
  let rec loop history =
    print_string "<client> ";
    flush stdout;
    match read_line () with
    | exception End_of_file ->
      print_endline "";
      print_endline "Bye."
    | input ->
      let line = String.trim input in
      if line = "" then loop history
      else if line = "/help" then (
        print_help ();
        loop history
      )
      else if line = "/quit" || line = "/exit" then
        print_endline "Bye."
      else
        let user_message = { C.role = User; parts = [line] } in
        match C.generate_content ?url ~key ~model ~messages:history () with
        | Error msg ->
          prerr_endline ("Gemini error: " ^ msg);
          loop history
        | Ok text ->
          Printf.printf "<gemini> %s\n%!" text;
          let model_message = { C.role = Model; parts = [text] } in
          loop (history @ [ user_message; model_message ])
  in
  loop history

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
  let key =
    match !api_key with
    | Some key -> key
    | None -> Sys.getenv "GEMINI_API_KEY"
  in
  loop ?url:!url ~key ~model:!model []
