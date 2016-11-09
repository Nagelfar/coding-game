module Poker.Domain

open Logging

open Poker.Model

type State = {
    Token: Token 
    Players: Player list }
    with
    static member initial = {
        Token = Token("")
        Players =[]
         }

let joinGame (gameToken: Token) state = 
    [ JoinGame ( {game = gameToken }) ] 

let startRound gameNumber players state =
    []

let processManager = 
    function 
    | RoundStarting gameToken -> joinGame gameToken
    | RoundStarted (gameNumber, players)-> startRound gameNumber players
    | e -> 
        log "warn" (sprintf "Ignoring %A" e)
        let emptyCommands:Command list = []
        let ignore state =emptyCommands
        ignore


let handle (c:Command)(a:State) =
    [RoundStarting(a.Token)]
    // function 
    // | GameStarting command -> joinGame command

let evolve state= 
    function
    | RoundStarting e -> 
        { Token = e
          Players = []}
    | RoundStarted (number, players) ->
        { state with 
            Players = players
        }
    | _ -> state