//The Frame Project 

open System
open System.IO
open Parser
open FrameInterpreter
open FrameParser

[<EntryPoint>]
let main argv =

    if Array.length argv <> 1 then 
        printf "Usage: dotnet run filename.fr\nTry running yourfile.fr instead.\n"
        exit 1

    try 
     File.ReadAllText (argv.[0]) |> ignore
    with ex -> 
     printfn "File %s Not Found\n\nEXITING" (argv.[0])
     exit 1

    let content = File.ReadAllText (argv.[0])

    let body = parse content

    buildHTML body

    printfn "Success! Check frame.html"
    0
   
