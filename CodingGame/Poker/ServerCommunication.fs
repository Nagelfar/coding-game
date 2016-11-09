module Poker.ServerCommunication 

open Logging

let translate (command: Command) = 

    let tokenToString (Token t) = t

    match command with 
    | Register x -> Some("REGISTER;"+x)
    | JoinGame g -> Some("JOIN;"+ tokenToString g.game )
    | a ->
        log "warn" (sprintf "Could not translate command %A" a)
        None

let interpret (message:string) =
    let parts = message.Split [|';'|]

    match parts with
    | [|"REGISTERED"|] -> Some(Registered)
    | [|"ROUND STARTING";token|] ->  Some(RoundStarting(Token(token)))
    | [|"ROUND STARTED";rundennummer;players|] ->
        let runde =  Rundennummer(rundennummer |>  int) 
        let playerList = players.Split [|','|] |> Array.toList |> List.map Player

        Some(RoundStarted(runde, playerList ))
    | unmatched -> 
        log "warn" (sprintf "Could not interpret event %A" unmatched)
        None