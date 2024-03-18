open Eio.Std
open Chat
open Chat.Common

let%test "1 client" =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let stdin = Eio_mock.Flow.make "stdin" in
  let net = Eio_mock.Net.make "net" in
  Eio_mock.Flow.on_read stdin
    [ `Return "hi client"; `Return "hi client2"; `Raise End_of_file ];
  let listening_socket = Eio_mock.Net.listening_socket "tcp/80" in
  let msg = make_user_message ~origin:0 ~content:"hi server" () |> to_cstruct in
  Eio_mock.Net.on_listen net [ `Return listening_socket ];
  Eio_mock.Backend.run @@ fun _ ->
  Switch.run ~name:"input" @@ fun sw ->
  Server.server ~stdin ~net ~addr |> ignore;
  let flow = Eio.Net.connect ~sw net addr in
  let _ = Eio.Flow.read_all flow in
  msg +> flow;
  true
