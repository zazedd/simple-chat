open Eio.Std
open Common

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

(* Breaks apart the ip string into the byte representation
   needed by the Ipaddr module *)
let string_to_ip str =
  let parts = String.split_on_char '.' str in
  let bytes = List.map int_of_string parts in
  let byte_to_char b = Char.chr b in
  String.of_seq (Seq.map byte_to_char (List.to_seq bytes))

(* Addr of string. Handles both extensive ips, just port numbers or an enter.
   Example:
   192.168.0.1:8080 -> `Tcp (192.168.0.1, 8080)`
   7000 -> `Tcp (127.0.0.1, 7000)`
   \n -> `Tcp (127.0.0.1, 8080)`
*)
let addr_of_string str =
  let l = String.trim str |> String.split_on_char ':' in
  match l with
  | [ "" ] -> `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)
  | [ p ] ->
      let port = int_of_string p in
      `Tcp (Eio.Net.Ipaddr.V4.loopback, port)
  | [ ip; p ] ->
      let listen_address = string_to_ip ip in
      let port = int_of_string p in
      `Tcp (Eio.Net.Ipaddr.of_raw listen_address, port)
  | _ -> failwith "Incorrect IP Address"

(* Sets up the server connection and starts the fibers for
   handling both input and incoming messages *)
let client ~stdin ~net =
  Logs.app (fun m ->
      m
        "Addr (ex: 192.168.0.1:8080), just the port (8080), or Enter \
         (127.0.0.1:8080):")
  |> ignore;
  let addr = addr_of_string (read_line ()) in
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
  let flow = Eio.Net.connect ~sw net addr in
  (* an int is 63 bytes in OCaml *)
  let buf = Cstruct.create 63 in
  let id =
    match Eio.Flow.single_read flow buf with
    | 0 -> failwith "Coudln't get client number. Aborting"
    | n ->
        let cstr = Cstruct.sub buf 0 n in
        from_cstruct cstr
  in
  (* The flows Hashtable for the clients will only contain the client <-> server flow.
     This allows us to abstract some code in common with the server,
     and possibly in the future add one-on-one connections with other clients *)
  let ht = Hashtbl.create 1 in
  Hashtbl.add ht 0 flow;
  let client_entity = { id; flows = ht } in
  Fiber.fork ~sw (fun () -> handle_input ~id ~stdin client_entity);
  Fiber.fork ~sw (fun () -> handle_incoming flow client_entity)
