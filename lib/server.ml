open Eio.Std
open Common

let client_num = ref 0

(* Handles any incoming message *)
let handle_incoming flow entity =
  let rec h () =
    let buf = Cstruct.create 2000 in
    match Eio.Flow.single_read flow buf with
    | 0 -> h ()
    | n -> (
        let got_at = Unix.gettimeofday () in
        let cstr = Cstruct.sub buf 0 n in
        let message = from_cstruct cstr in
        match message with
        | User m ->
            handle_user_message m flow entity.id got_at;
            Hashtbl.iter
              (fun k fl -> if k <> m.origin then cstr +> fl)
              entity.flows;
            h ()
        | Confirmation { receiver_id; time_took } ->
            Logs.info (fun m ->
                m "Message received by Client %d | Time took: %fs" receiver_id
                  time_took)
            |> ignore;
            h ()
        | Exit client_num ->
            traceln "See you later, Client %d" client_num;
            Hashtbl.remove entity.flows client_num;
            Eio.Net.close flow)
  in
  h ()

(* Prepares for a new client and starts accepting their incoming messages*)
let run server_entity fl _ =
  client_num := !client_num + 1;
  Hashtbl.add server_entity.flows !client_num fl;
  Logs.info (fun m -> m "Client %d connected" !client_num) |> ignore;
  (* Inform the client of its client number *)
  to_cstruct !client_num +> fl;
  handle_incoming fl server_entity

let server ~stdin ~net ~addr =
  Switch.run ~name:"server" @@ fun sw ->
  let sock = Eio.Net.listen net ~sw ~backlog:1 ~reuse_addr:true addr in
  let server_entity = { id = 0; flows = Hashtbl.create 0 } in
  Fiber.fork ~sw (fun () -> handle_input ~stdin server_entity);
  Eio.Net.run_server sock (run server_entity)
    ~on_error:(traceln "Server error: %a" Fmt.exn)
