module CodingGame

open System

open Logging

open Connectivity
open EventStore.ToyInMemoryEventStore 

open Poker
open Poker.Model
open Poker.Domain
open Poker.CommandHandlers
open Poker.ServerCommunication


// let eventHandler = new EventHandler()

let eventHandler (event:Event) =
    let eventDescription = sprintf "Saved %A" event
    logInfo eventDescription |> ignore

let store =
    create()
    |> subscribe eventHandler
    // |> subscribe eventHandler.Handle

let handle = Commandhandler.create (readStream store) (appendToStream store)
let handleProcess = Commandhandler.processManager (readStream store)

let processManagers = 
    function 
    | Registered ->  
        // no action needed for this one
        logInfo (sprintf "Registriert!")
        []
    | e -> handleProcess(e)

let dispatchServerEvent dispatchCommandToServer (serverEvent:Event) = 
    let commands = processManagers serverEvent
    
    // commands 
    // |> List.iter handle
    commands
    |> List.iter dispatchCommandToServer
    

let serverEventHandler commandDispatcher serverEvent=
   commandDispatcher serverEvent
    // function 
    // | Registered -> printfn "Registriert!"
    // | RoundStarting e ->
    //     printfn "Runde startet %A" e  
    //     commandDispatcher(RoundStarting(e))
        // let events = handle (StartNewGame({game=e}))
        // let 

let ServerCommandDispatcher sender command =
    sender command


[<EntryPoint>]
let main argv =    
    let socket = initialize(9000)

    let sender = send socket ServerCommunication.translate
    let receiver = fun ()-> receive socket

    let dispatchToServer = ServerCommandDispatcher sender
    let eventHandlers = serverEventHandler  (dispatchServerEvent dispatchToServer)

    let register  = Register("fm")
    
    sender register

    let dispatcher = dispatch ServerCommunication.interpret eventHandlers

    messageLoop receiver dispatcher 

    Console.ReadLine()|>ignore

    0 // return an integer exit code


