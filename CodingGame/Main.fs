module CodingGame

open System

open Logging

open Connectivity
open EventStore.ToyInMemoryEventStore 

open BlackJack
open BlackJack.Model

open BlackJack.ServerCommunication


// let eventHandler = new EventHandler()

// let eventHandler (event:Event) =
//     let eventDescription = sprintf "Saved %A" event
//     logInfo eventDescription |> ignore

// // let store =
//     create()
    // |> subscribe eventHandler
    // |> subscribe eventHandler.Handle

// let handle = Commandhandler.create (readStream store) (appendToStream store)
// let handleProcess = Commandhandler.processManager (readStream store)

// let processManagers = 
//     function 
//     | Ok ->  
//         // no action needed for this one
//         logInfo (sprintf "Registriert!")
//         []
//     | e -> handleProcess(e)

// let dispatchServerEvent dispatchCommandToServer (serverEvent:Event) = 
//     let commands = processManagers serverEvent
    
    // commands 
    // |> List.iter handle
    // commands
    // |> List.iter dispatchCommandToServer
    

// let serverEventHandler commandDispatcher serverEvent=
//    commandDispatcher serverEvent
//     function 
//     | Registered -> printfn "Registriert!"
//     | RoundStarting e ->
//         printfn "Runde startet %A" e  
//         commandDispatcher(RoundStarting(e))
//         let events = handle (StartNewGame({game=e}))
//         events

let mutable geld = 100
let mutable hand :Hand= []
let mutable handValue :HandValue= {
    aceLow=0; aceHigh=0
}

let mutable bank :Hand = [] 
let mutable bankValue :HandValue= {
    aceLow=0; aceHigh=0
}

let logHand (hand:Hand) value =
    let hands = List.fold (fun agg x -> (sprintf "%A %A" agg x)) "" hand    
    let combined = sprintf "%A %A" hands value

    logInfo combined


let calculateBet = 
    let bet = geld / 10
    geld <- geld - bet
    bet

let cardRecieved (card: Card) = 
    hand <- card::hand    
    handValue <- computeHand hand
    logInfo "hand"
    logHand hand handValue

let bankRecieved (card:Card) =
    bank <- card::bank
    bankValue <- computeHand bank
    logInfo "bank"
    logHand bank bankValue

let playername = "fm"
let decideStayOrGo =
    let aces = hand |> List.filter (fun c -> c = Ace) |> List.length

    if(handValue.aceHigh < 11 || handValue.aceLow <= 10 ) then
        PickCard(playername)
    else if(aces > 1 && handValue.aceHigh < 17) then
        PickCard(playername)
    else
        Stay(playername)
    

let serverEventHandler (commandDispatcher: Command->Unit) serverEvent =
    let command = match serverEvent with
                    | Set -> Some(SetGeld(calculateBet, playername))
                    | CardRecived c -> 
                        cardRecieved c
                        None
                    | BankRecived b ->
                        bankRecieved b
                        None
                    | StayOrCard -> 
                        let aces = hand |> List.filter (fun c -> c = Ace) |> List.length

                        if(handValue.aceHigh < 11 || handValue.aceLow <= 10 ) then
                            Some(PickCard(playername))
                        else if(aces > 1 && handValue.aceHigh < 17) then
                            Some(PickCard(playername))
                        else
                            Some(Stay(playername))
                    | Money m ->  
                        geld <- m
                        None
                    | RoundStarting -> 
                        bank <- []
                        bankValue <- {
                            aceLow=0; aceHigh=0
                        }
                        hand <- []
                        handValue <- {
                            aceLow=0; aceHigh=0
                        }
                        None
                    | _ -> None

    if( command.IsSome) then
        commandDispatcher (command.Value)
    

let ServerCommandDispatcher sender command =
    sender command

// let ee2 toserver event =
//     toserver

[<EntryPoint>]
let main argv =    
    let socket = initialize(22040)

    let sender = send socket ServerCommunication.translate
    let receiver = fun ()-> receive socket

    let dispatchToServer = ServerCommandDispatcher sender
    let eventHandlers = serverEventHandler  dispatchToServer

    let register  = Join(playername)
    
    sender register

    let dispatcher = dispatch ServerCommunication.interpret eventHandlers

    messageLoop receiver dispatcher 

    Console.ReadLine()|>ignore

    0 // return an integer exit code


