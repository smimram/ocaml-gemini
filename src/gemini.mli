module Client : sig
  type t

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

  val create : ?base_url:string -> ?api_key:string -> unit -> t

  val make_request :
    ?generation_config:generation_config ->
    model:string ->
    message list ->
    request

  val generate_content : t -> request -> (response, string) result
end