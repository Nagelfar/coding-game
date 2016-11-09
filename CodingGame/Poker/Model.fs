[<AutoOpen>]
module Poker.Model

type Token =
    | Token of string

type Player =
    | Player of string

type Rundennummer =
    | Rundennummer of int

type Command =
    | Register of string
    | JoinGame of JoinGame
    
and JoinGame= {
    game: Token
}

type Event =
    | Registered
    | RoundStarting of Token
    | RoundStarted of Rundennummer * Player list 
    
// and GameStarting = { game: Token } 
    // | GameStarted of 