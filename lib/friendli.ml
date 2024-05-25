let request ~meth ?body env ~friendli_token path =
  let headers =
    [ "authorization", "Bearer " ^ friendli_token
    ; "user-agent", "Friendli Discord"
    ; "content-Type", "application/json"
    ; "accept", "application/json"
    ]
  in
  let url = "https://inference.friendli.ai/v1" ^ path in
  Logs.info (fun m ->
    m
      "REST request: %s %s [%s]"
      (Cohttp.Code.string_of_method meth)
      url
      (body |> Option.fold ~none:"" ~some:Yojson.Safe.to_string));
  let body = body |> Option.map (fun x -> `Fixed (Yojson.Safe.to_string x)) in
  Eio.Switch.run
  @@ fun sw ->
  let resp = Discord__Httpx.Http.request ~meth ~headers ?body env ~sw url in
  let body = Discord__Httpx.Http.drain_resp_body resp in
  let body =
    try body |> Yojson.Safe.from_string |> Option.some with
    | _ -> None
  in
  Cohttp.Response.status (fst resp), body
;;

let request (env : Eio_unix.Stdenv.base) ~friendli_token name ~meth ?body url =
  match request env ~friendli_token ~meth ?body url with
  | code, Some body when Cohttp.Code.(code |> code_of_status |> is_success) ->
    Logs.info (fun m -> m "%s: %s" name (Yojson.Safe.to_string body));
    (try Ok body with
     | exn -> Error (Printf.sprintf "%s: %s" name (Printexc.to_string exn)))
  | code, body ->
    Error
      (Printf.sprintf
         "%s: %s: %s"
         name
         (Cohttp.Code.string_of_status code)
         (body |> Option.fold ~none:"" ~some:Yojson.Safe.to_string))
  | exception exn -> Error (Printf.sprintf "%s failed: %s" name (Printexc.to_string exn))
;;

module M = struct
  type msg =
    { name : string
    ; meth : Cohttp.Code.meth
    ; body : Yojson.Safe.t option
    ; url : string
    }

  type reply = (Yojson.Safe.t, string) result
  type param = { friendli_token : string }

  let process (env : Eio_unix.Stdenv.base) { friendli_token; _ } { name; meth; body; url }
    : reply
    =
    request env ~friendli_token name ~meth ?body url
  ;;
end

open Discord__Rate_limiter.Make (M)

type nonrec t = t

let start env ~sw ~max_running ~friendli_token =
  start env ~sw ~max_running M.{ friendli_token }
;;

let request name ~meth ?body url (t : t) =
  rate_limited_process M.{ name; meth; body; url } t
;;

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type user_message =
  { role : string [@default "user"]
  ; content : string option
  }
[@@deriving yojson, show, make]

type command =
  { type_ : string [@key "type"]
  ; enum : string list
  ; description : string
  }
[@@deriving make, yojson]

type message'' =
  { type_ : string [@key "type"]
  ; description : string
  }
[@@deriving make, yojson]

type properties =
  { command : command
  ; message'' : message'' [@key "message"]
  }
[@@deriving make, yojson]

type parameters =
  { type_ : string [@key "type"]
  ; properties : properties
  ; required : string list
  }
[@@deriving make, yojson]

type function_ =
  { name : string
  ; description : string
  ; parameters : parameters
  }
[@@deriving make, yojson]

type tool =
  { type_ : string [@key "type"]
  ; function_ : function_ [@key "function"]
  }
[@@deriving make, yojson]

(* https://docs.friendli.ai/openapi/create-chat-completions/ *)
type create_message_param =
  { model : string
  ; messages : user_message list
  ; tools : tool list option [@yojson.option]
  ; frequency_penalty : float option [@yojson.option]
  ; presence_penalty : float option [@yojson.option]
  ; max_tokens : int [@default 200]
  ; n : int option [@yojson.option]
  ; stop : string list option [@yojson.option]
  ; stream : bool option [@yojson.option]
  ; temperature : float option [@yojson.option]
  ; top_p : float option [@yojson.option]
  ; timeout_microseconds : int option [@yojson.option]
  }
[@@yojson.allow_extra_fields] [@@deriving make, yojson]

let create_message t p =
  let open Yojson.Safe.Util in
  request
    "create_message"
    ~meth:`POST
    ~body:(yojson_of_create_message_param p)
    "/chat/completions"
    t
  |> Result.map (fun r ->
    r
    |> member "choices"
    |> to_list
    |> List.map (fun choice ->
      choice |> member "message" |> member "content" |> to_string)
    |> String.concat "\n")
;;

let create_q t p =
  let open Yojson.Safe.Util in
  request
    "create_message"
    ~meth:`POST
    ~body:(yojson_of_create_message_param p)
    "/chat/completions"
    t
  |> Result.map (fun r ->
    let rs =
      r
      |> member "choices"
      |> to_list
      |> List.hd
      |> member "message"
      |> member "tool_calls"
      |> to_list
      |> List.hd
      |> member "function"
      |> member "arguments"
    in
    let command = "!" ^ (rs |> member "command" |> to_string) in
    let message = rs |> member "message" |> to_string in
    command ^ message)
;;
