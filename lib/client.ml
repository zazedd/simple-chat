open Eio.Std
open Common

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
            h ()
        | Confirmation { receiver_id; time_took } ->
            assert (receiver_id = 0);
            Logs.info (fun m ->
                m "Message received by Server | Time took: %fs" time_took)
            |> ignore;
            h ()
        | Exit _ ->
            traceln "See you space cowboy...";
            Eio.Net.close flow;
            exit 0)
  in
  h ()

let client ~stdin ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
  let flow = Eio.Net.connect ~sw net addr in
  let buf = Cstruct.create 63 in
  (* an int is 63 bytes *)
  let id =
    match Eio.Flow.single_read flow buf with
    | 0 -> failwith "Coudln't get client number. Aborting"
    | n ->
        let cstr = Cstruct.sub buf 0 n in
        from_cstruct cstr
  in
  let ht = Hashtbl.create 1 in
  Hashtbl.add ht 0 flow;
  let client_entity = { id; flows = ht } in
  Fiber.fork ~sw (fun () -> handle_input ~id ~stdin client_entity);
  Fiber.fork ~sw (fun () -> handle_incoming flow client_entity)
