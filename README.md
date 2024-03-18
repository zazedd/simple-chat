# Simple chat

### OCaml version

The OCaml version used was 5.1.1.

### Dependencies
- eio 
- logs
- ppx_deriving
- ppx_inline_test

### Running:

`dune exec chat <option>`

`option` can be one of the following:  
`s` or `server`; `c` or `client`

### Functionalities

A client can read and write text messages.  

The server can see when a client connects, when a client disconnects.  
When it receives a message from a client it broadcasts it to other connected clients.  
It can also read and send text messages to the clients itself.

If all goes well, the client should get a message of this type:  
`Message received by Server | Time took: 0.000440s`
This message confirms that the server has received the message.  
The server also gets confirmation on which clients received messages:  
`Message received by Client 2 | Time took: 0.001320s`

#### Quitting
Client: `CTRL+D` closes the connection and quits. The server stays on and other clients stay connected.  
Server: `CTRL+D` closes all connections and quits. Also signals all connected clients to shutdown.

### Testing:

```dune runtest```

Unfortunately I couldn't get tests to work properly, the server's stdin always gets an EOF and finishes
before I can send a message through a client flow.

### Project Structure
```
├── bin
│   ├── dune
│   └── main.ml       -> Executable
├── lib
│   ├── client.ml     -> Client specific code
│   ├── common.ml     -> Common functions, types and records used in both the server and client
│   ├── dune
│   └── server.ml     -> Server specific code
└── test
    ├── dune
    └── test_chat.ml  -> Incomplete unit test
```

