//The Frame Project 

open System
open System.IO
open Parser
open FrameInterpreter
open FrameParser

[<EntryPoint>]
let main argv =

    if Array.length argv <> 1 then 
        printf "Usage: dotnet run filename.fr\n"
        exit 1

    let content = argv.[0]

    let body = go content

    let body = buildHTML body
    printfn "%s" body 
    0    
