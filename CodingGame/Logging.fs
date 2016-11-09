module Logging
open System
open System.IO

let mutable private gameTokenFileName:string = "none"

let private log_ext = ".log"
let private log_path = "./logs"

let private logFileName() = sprintf "%s/log_%s%s" log_path gameTokenFileName log_ext

let private newLogFileStream() = 
    if Directory.Exists(log_path) |> not then Directory.CreateDirectory(log_path) |> ignore
    let str = new StreamWriter(File.OpenWrite(logFileName()))
    str.AutoFlush <- true
    str

let private logToFile file msg =
    let str =
        match !file with
        | None -> 
            let str = newLogFileStream()
            file := Some str
            str
        | Some str -> str
    str.WriteLine(msg:string)

let private closeLog file =
    match !file with
    | None -> ()
    | Some (str:StreamWriter) -> str.Flush(); str.Close()
    file := None

let private rollover file =
    closeLog file
    file := newLogFileStream() |> Some

type private LogMsg =  Log of string | CloseLog of AsyncReplyChannel<unit> | Rollover


let private logProcessor (inbox:MailboxProcessor<LogMsg>)  =
    let file = ref None
    async {
        while true do 
            try
                let! msg = inbox.Receive()
                match msg with
                | Log s     -> 
                    logToFile file s
                    Console.WriteLine s
                | Rollover -> 
                    rollover file
                | CloseLog rc -> 
                    closeLog file
                    rc.Reply()
            with ex -> Console.WriteLine ex.Message
            }

let private logAgent = 
    let mb = MailboxProcessor.Start logProcessor
    
    mb

//API

let changeGameFileName game =
    gameTokenFileName <- game
    logAgent.Post (Rollover)

let log (tag:string) (desc:string) = 
    let msg = sprintf "%A %s : %s" DateTime.Now tag desc 
    logAgent.Post (Log msg)   

let logInfo (desc:string) = 
    log "info" desc

let logVerbose (desc:string) = 
    log "verbose" desc

let logex (tag:string) (ex:Exception) = 
    log tag ex.Message
    let msg = sprintf "%s" ex.StackTrace
    logAgent.Post (Log msg)   


let terminateLog() = logAgent.PostAndReply(fun rc -> CloseLog rc)