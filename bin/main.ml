open! Base

type command =
  | Help
  | Ping
  | Join
  | Leave
  | Play of string (* youtube url *)
  | Say of string (* message *)
  | Weather of string
  | Q of string
  | Chat of string (* message *)

type db =
  { model : string
  ; temperature : float
  ; response_max_tokens : int
  ; persona : string
  }
[@@deriving yojson, show, make]

let parse_config content : db =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string content in
  let model = json |> member "model" |> to_string in
  let temperature = json |> member "temperature" |> to_float in
  let response_max_tokens = json |> member "response_max_tokens" |> to_int in
  let persona = json |> member "persona" |> to_string in
  Stdlib.print_endline @@ Printf.sprintf "%s" model;
  { model; temperature; response_max_tokens; persona }
;;

let load_config () =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "db.json" in
  parse_config (Eio.Path.load path)
;;

(* (\* Find last character from both single-byte (like ASCII) and multi-byte (like Korean) characters *\) *)
(* let last_char_in_str s = *)
(*   let rec loop pos = *)
(*     if pos = 0 *)
(*     then pos *)
(*     else if Char.to_int s.[pos] land 0xC0 *)
(*             <> 0x80 (\* bits `10` indicates the start of UTF-8 character. *\) *)
(*     then pos *)
(*     else loop (pos - 1) *)
(*   in *)
(*   let size = String.length s in *)
(*   if size = 0 *)
(*   then "" *)
(*   else ( *)
(*     let start = loop (size - 1) in *)
(*     s |> String.sub ~pos:start ~len:(size - start)) *)
(* ;; *)

(* let byte3_to_unicode s = *)
(*   match s |> String.to_list |> List.map ~f:Char.to_int |> List.rev with *)
(*   | [ b3; b2; b1 ] -> *)
(*     (\* Reference: https://en.wikipedia.org/wiki/UTF-8#Encoding *\) *)
(*     let unicode = ((b1 land 0x0F) lsl 12) lor ((b2 land 0x3F) lsl 6) lor (b3 land 0x3F) in *)
(*     Some unicode *)
(*   | _ -> None *)
(* ;; *)

(* let ends_with_last_consonant s = *)
(*   let is_hangul = *)
(*     let low = Int.of_string "0xAC00" in *)
(*     let high = Int.of_string "0xD7AF" in *)
(*     Int.between ~low ~high *)
(*   in *)
(*   let has_last_consonant hangul_unicode = (hangul_unicode - 44032) % 28 > 0 in *)
(*   let unicode_opt = s |> last_char_in_str |> byte3_to_unicode in *)
(*   Option.( >>| ) unicode_opt (fun unicode -> *)
(*     if unicode |> is_hangul then unicode |> has_last_consonant else false) *)
(* ;; *)

let parse_command s =
  let concat_with_space = String.concat ~sep:" " in
  let parsed = String.split ~on:' ' s in
  match parsed with
  | [ "!도움말" ] -> Ok Help
  | [ "!핑" ] -> Ok Ping
  | [ "!입장" ] -> Ok Join
  | [ "!퇴장" ] -> Ok Leave
  | [ "!재생"; url ] -> Ok (Play url)
  | "!말" :: tl -> Ok (Say (concat_with_space tl))
  | "어이" :: tl -> Ok (Chat (concat_with_space tl))
  | "!날씨" :: tl -> Ok (Weather (concat_with_space tl))
  | "!Q" :: tl -> Ok (Q (concat_with_space tl))
  | _ -> Error "Invalid command"
;;

let handle_event ?friendli _env ~sw agent rest state =
  let open Discord.Event in
  function
  | Dispatch (MESSAGE_CREATE msg) ->
    let guild_id = Option.value_exn msg.guild_id in
    let say text =
      Logs.info (fun m -> m "Saying %s" text);
      Discord.Rest.make_create_message_param ~content:text ()
      |> Discord.Rest.create_message msg.channel_id rest
    in
    (match parse_command msg.content with
     | Ok Help ->
       Logs.info (fun m -> m "help");
       let help_msg =
         "!도움말 -> 도움말\n\
          !핑 -> 퐁\n\
          !입장 -> 보이스 채널에 초대\n\
          !퇴장 -> 보이스 채널에서 퇴장\n\
          !재생 { 유튜브_링크 } -> 음성 재생\n\
          !말 { 메세지 } -> 말해요\n\
          어이 { 메세지 } -> AI 채팅\n"
       in
       if Discord.Rest.make_create_message_param
            ~embeds:[ Discord.Entity.make_embed ~description:help_msg () ]
            ()
          |> Discord.Rest.create_message msg.channel_id rest
          |> Result.is_error
       then Logs.err (fun m -> m "Failed to send help message");
       state
     | Ok Ping ->
       Logs.info (fun m -> m "ping");
       if Discord.Rest.make_create_message_param
            ~embeds:[ Discord.Entity.make_embed ~description:"퐁" () ]
            ()
          |> Discord.Rest.create_message msg.channel_id rest
          |> Result.is_error
       then Logs.err (fun m -> m "Failed to send pong");
       state
     | Ok Join ->
       (match
          agent |> Discord.Agent.get_voice_states ~guild_id ~user_id:msg.author.id
        with
        | None -> ()
        | Some vstate ->
          (match vstate.Discord.Event.channel_id with
           | None -> ()
           | Some channel_id -> agent |> Discord.Agent.join_channel ~guild_id ~channel_id));
       state
     | Ok Leave ->
       agent |> Discord.Agent.leave_channel ~guild_id;
       state
     | Ok (Play url) ->
       Logs.info (fun m -> m "Playing %s" url);
       agent |> Discord.Agent.play_voice ~guild_id ~src:(`Ytdl url);
       state
     | Ok (Say text) ->
       if text |> say |> Result.is_error then Logs.info (fun m -> m "Failed to say");
       state
     | Ok (Weather city) ->
       Logs.info (fun m -> m "Getting weather of %s" city);
       let wttr = Friendli__Wttr.start _env ~sw ~max_running:5 in
       let answer_r = Friendli__Wttr.create_message wttr city in
       if answer_r
          |> Result.bind ~f:(fun answer ->
            Discord.Rest.make_create_message_param ~content:answer ()
            |> Discord.Rest.create_message msg.channel_id rest)
          |> Result.is_error
       then Logs.err (fun m -> m "Failed to send chat response");
       state
     | Ok (Chat text) ->
       Logs.info (fun m -> m "Chatting %s" text);
       (match friendli with
        | None -> Logs.err (fun m -> m "FriendliAI not set properly.")
        | Some friendli ->
          let { model; temperature; response_max_tokens; persona } = load_config () in
          let max_tokens =
            let prompt_tokens =
              let chars_per_token = 4 in
              String.length persona / chars_per_token
            in
            prompt_tokens + response_max_tokens
          in
          let answer_r =
            Friendli.make_create_message_param
              ~model
              ~messages:
                [ Friendli.make_user_message ~role:"system" ~content:persona ()
                ; Friendli.make_user_message ~content:text ()
                ]
              ~max_tokens
              ~temperature
              ()
            |> Friendli.create_message friendli
          in
          if answer_r
             |> Result.bind ~f:(fun answer ->
               Discord.Rest.make_create_message_param ~content:answer ()
               |> Discord.Rest.create_message msg.channel_id rest)
             |> Result.is_error
          then Logs.err (fun m -> m "Failed to send chat response"));
       state
     | Ok (Q text) ->
       Logs.info (fun m -> m "Chatting %s" text);
       (match friendli with
        | None -> Logs.err (fun m -> m "FriendliAI not set properly.")
        | Some friendli ->
          let { model; temperature; _ } = load_config () in
          let max_tokens = 1000 in
          let required : string list = [ "command" ] in
          let message'' : Friendli.message'' =
            { type_ = "string"
            ; description =
                "arguments for given command. Expected answer: city(e.g., Boston, LA)  \
                 for weather(날씨), name of the timezone(시간대)(e.g., Asia/Seoul) for time"
            }
          in
          let enum : string list = [ "날씨"; "시간대" ] in
          let description = "Command prefix to be used in Discord" in
          let command : Friendli.command = { type_ = "string"; enum; description } in
          let properties : Friendli.properties = { command; message'' } in
          let parameters : Friendli.parameters =
            { type_ = "object"; properties; required }
          in
          let function_ : Friendli.function_ =
            { name = "get_discord_command"
            ; description = "Get the proper discord command"
            ; parameters
            }
          in
          let tool : Friendli.tool = { type_ = "function"; function_ } in
          let tools = [ tool ] in
          let answer_r =
            Friendli.make_create_message_param
              ~model
              ~messages:[ Friendli.make_user_message ~content:text () ]
              ~tools
              ~max_tokens
              ~temperature
              ()
            |> Friendli.create_message friendli
          in
          if answer_r
             |> Result.bind ~f:(fun answer ->
               Logs.info (fun m -> m "Chatting %s" answer);
               Discord.Rest.make_create_message_param ~content:answer ()
               |> Discord.Rest.create_message msg.channel_id rest)
             |> Result.is_error
          then Logs.err (fun m -> m "Failed to send chat response"));
       state
     | _ -> state)
  | _ -> state
;;

let () =
  Dotenv.export () |> ignore;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let token = Sys.getenv_exn "DISCORD_TOKEN" in
  let youtubedl_path = Sys.getenv "YOUTUBEDL_PATH" in
  let ffmpeg_path = Sys.getenv "FFMPEG_PATH" in
  let friendli_token = Sys.getenv "FRIENDLI_TOKEN" in
  let intents =
    Discord.Intent.encode [ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES; MESSAGE_CONTENT ]
  in
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
  @@ fun () ->
  Eio.Switch.run
  @@ fun sw ->
  let open Friendli in
  let friendli =
    friendli_token
    |> Option.map ~f:(fun friendli_token -> start env ~sw ~max_running:5 ~friendli_token)
  in
  let _consumer : _ Discord.Consumer.t =
    Discord.Consumer.start
      env
      ~sw
      ~token
      ~intents
      ?ffmpeg_path
      ?youtubedl_path
      (fun () -> ())
      (handle_event ?friendli)
  in
  ()
;;
