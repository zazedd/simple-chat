open Chat

exception InvalidChoice of string

let () =
  let choice = Sys.argv.(1) in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level @@ Some Logs.Debug;
  let r env =
    match choice with
    | "s" | "server" ->
        Server.server ~stdin:(Eio.Stdenv.stdin env) ~net:(Eio.Stdenv.net env)
          ~addr
    | "c" | "client" ->
        Client.client ~stdin:(Eio.Stdenv.stdin env) ~net:(Eio.Stdenv.net env)
          ~addr
    | _ ->
        InvalidChoice "Available options are: s (server), c (client)" |> raise
  in
  Eio_main.run @@ fun env -> r env |> ignore
