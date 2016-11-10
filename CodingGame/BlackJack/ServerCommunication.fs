module BlackJack.ServerCommunication 

open Logging

let translate (command: Command) = 

    let tokenToString (Token t) = t

    match command with
     
    | Join x -> Some("JOIN;"+x)
    | SetGeld (betrag, p)-> Some("SET;"+(sprintf "%A" betrag) + ";"+ p)
    | Stay p -> Some("STAY;"+p)
    | PickCard p -> Some("CARD;"+p)
    // | JoinGame g -> Some("JOIN;"+ tokenToString g.game )
    | a ->
        log "warn" (sprintf "Could not translate command %A" a)
        None

let interpret (message:string) =
    let parts = message.Split [|';'|]

    match parts with
    | [|"OK"|] -> Some(Ok)
    | [|"ROUND STARTING"|] ->  Some(RoundStarting)
    | [|"SET"|] ->  Some(Set)
    | [|"CARD";card|] ->  Some(CardRecived(parseCard card))
    | [|"BANK";card|] ->  Some(BankRecived(parseCard card))
    | [|"STAY_OR_CARD"|] ->  Some(StayOrCard)
    | [|"MONEY";money|] ->  Some(Money(money |> int))
    // | [|"ROUND ENDED";playerString|] ->
    //     let players = playerString.Split
    //     Some(Money(money |> int))
    
    
    
    // | [|"ROUND STARTED";rundennummer;players|] ->
        // let runde =  Rundennummer(rundennummer |>  int) 
        // let playerList = players.Split [|','|] |> Array.toList |> List.map Player

        // Some(RoundStarted(runde, playerList ))
    | unmatched -> 
        log "warn" (sprintf "Could not interpret event %A" unmatched)
        None