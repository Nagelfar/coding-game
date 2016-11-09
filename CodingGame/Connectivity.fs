module Connectivity

open System
open System.Net
open System.Net.Sockets

open System.Text

open Logging

type ServerMessage = string

type ServerResonse = string

type Socket = { 
    Endpoint: IPEndPoint;
    Client: UdpClient
}

let initialize(port) = { 
    Endpoint = new IPEndPoint(IPAddress.Any, port);
    Client = new UdpClient()
}

let encode (message: string) = Encoding.Default.GetBytes(message)

let decode data = Encoding.Default.GetString(data)

let receive (socket: Socket) :ServerResonse=
    let data = socket.Client.Receive(ref socket.Endpoint)
    let decoded = decode(data)
    logVerbose (sprintf "Reveived %A" decoded)

    decoded

let send (socket: Socket) translate command=
    let translated = translate command

    match translated with 
    | Some(message) ->
        logVerbose (sprintf "Sending %A" message)
        let data = encode message         
        socket.Client.Send(data,data.Length,socket.Endpoint) |>ignore
    | None -> ()
let messageLoop (receiver : unit->ServerResonse) dispatcher = 
    while true do 
        let message = receiver()
        dispatcher(message)

let dispatch interpreter handlers message=
    let event = interpreter message
     
    match event with
    | Some(e)-> handlers e
    | None -> log "warn" (sprintf "Could not dispatch '%A'" message)