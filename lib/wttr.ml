let request ~meth ?body env path =
  let headers =
    [ "user-agent", "Friendli Discord"
    ; "content-Type", "application/json"
    ; "accept", "application/json"
    ]
  in
  let url = "https://wttr.in" ^ path in
  let body = body |> Option.map (fun x -> `Fixed x) in
  Eio.Switch.run
  @@ fun sw ->
  let resp = Discord__Httpx.Http.request ~meth ~headers ?body env ~sw url in
  let body = Discord__Httpx.Http.drain_resp_body resp in
  let body =
    try body |> Option.some with
    | _ -> None
  in
  Cohttp.Response.status (fst resp), body
;;

let request (env : Eio_unix.Stdenv.base) name ~meth ?body url =
  match request env ~meth ?body url with
  | code, Some body when Cohttp.Code.(code |> code_of_status |> is_success) ->
    (try Ok body with
     | exn -> Error (Printf.sprintf "%s: %s" name (Printexc.to_string exn)))
  | code, body ->
    Error
      (Printf.sprintf
         "%s: %s: %s"
         name
         (Cohttp.Code.string_of_status code)
         (body |> Option.fold ~none:"" ~some:(fun x -> x)))
  | exception exn -> Error (Printf.sprintf "%s failed: %s" name (Printexc.to_string exn))
;;

module M = struct
  type msg =
    { name : string
    ; meth : Cohttp.Code.meth
    ; body : string option
    ; url : string
    }

  type reply = (string, string) result
  type param = None

  let process (env : Eio_unix.Stdenv.base) _ { name; meth; body; url } : reply =
    request env name ~meth ?body url
  ;;
end

open Discord__Rate_limiter.Make (M)

type nonrec t = t

let start env ~sw ~max_running = start env ~sw ~max_running None

let request name ~meth ?body url (t : t) =
  rate_limited_process M.{ name; meth; body; url } t
;;

let create_message t p =
  request "create_message" ~meth:`GET (Printf.sprintf "/%s?format=3" p) t
;;
