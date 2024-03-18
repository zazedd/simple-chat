open Eio.Std
open Eio.Net

type 'a entity = { id : int; mutable flows : (int, 'a stream_socket) Hashtbl.t }

type user_message = {
  origin : int;
  content : string;
  sent_at : float; [@default Unix.gettimeofday ()]
}
[@@deriving make]

type message =
  | User of user_message
  | End_to_end of user_message * int (* user message and destination *)
  | Confirmation of { receiver_id : int; time_took : float }
  | Error of string
  | Exit of int (* id of the one who wants to quit *)

let to_cstruct msg =
  let b = Marshal.to_bytes msg [] in
  Cstruct.of_bytes b

let from_cstruct cstr = Marshal.from_bytes (Cstruct.to_bytes cstr) 0
let ( +> ) a b = Eio.Flow.copy (Eio.Flow.cstruct_source [ a ]) b

let pp_server_client id =
  match id with 0 -> Fmt.str "Server:" | n -> Fmt.str "Client %d:" n

let handle_input ?(id = 0) ~stdin entity =
  let buf = Eio.Buf_read.of_flow stdin ~initial_size:2000 ~max_size:100000 in
  let rec h () =
    try
      let line = Eio.Buf_read.line buf in
      let sp = String.split_on_char ' ' line in
      let cstr =
        if sp |> List.hd = "/msg" && List.length sp >= 3 then
          let content = List.tl sp |> List.tl |> String.concat " " in
          let destination = List.nth sp 1 |> int_of_string in
          End_to_end (make_user_message ~origin:id ~content (), destination)
          |> to_cstruct
        else User (make_user_message ~origin:id ~content:line ()) |> to_cstruct
      in
      Hashtbl.iter (fun _ fl -> cstr +> fl) entity.flows;
      h ()
    with End_of_file ->
      traceln "Exiting...";
      let cstr = Exit id |> to_cstruct in
      Hashtbl.iter (fun _ fl -> cstr +> fl) entity.flows;
      exit 0
  in
  h ()

let handle_user_message m flow receiver_id got_at =
  let out = pp_server_client m.origin in
  let timestamp = Unix.localtime m.sent_at in
  traceln "%02d:%02d:%02d - %s %s" timestamp.tm_hour timestamp.tm_min
    timestamp.tm_sec out m.content;
  (Confirmation { receiver_id; time_took = got_at -. m.sent_at } |> to_cstruct)
  +> flow
