module Poker.CommandHandlers

open Logging

open Poker.Domain

let mutable private gameTokenId:Token = Token("")

let changeToken token =
    gameTokenId <- token
    let tokenString (Token t) = t
    changeGameFileName (tokenString gameTokenId)
    gameTokenId

let gameId =
    function
    | JoinGame (token) -> changeToken (token.game)        
    | _ -> gameTokenId

let processId = 
    function
    | RoundStarting (token) -> changeToken token
    | _ -> gameTokenId

module Commandhandler =

 // this is the "repository"
    let streamId game = sprintf "game-%A" game
    let load readStream game =
        let rec fold state version =
            let events, lastEvent, nextEvent = readStream (streamId game) version 500
            let state = List.fold evolve state events
            match nextEvent with
            | None -> lastEvent, state
            | Some n -> fold state n
        fold State.initial 0


    // the mapsnd function works on a pair.
    // It applies the function on the second element.
    let inline mapsnd f (v,s) = v, f s
            
    let create readStream appendToStream =

        let load = load readStream

        let save game expectedVersion events = appendToStream (streamId game) expectedVersion events


        fun command ->
            let id = gameId command

            load id
            |> mapsnd (handle command)
            ||> save id

    let processManager readStream = 
        let load = load readStream
        
        let inline useCommands (v,c) = c

        fun event ->
            let id = processId event

            load id
            |> mapsnd (processManager event)
            |> useCommands
