//The Frame Project 

open System
open System.IO
open FrameInterpreter
open FrameParser

[<EntryPoint>]
let main argv =

    if Array.length argv <> 1 then 
        printf "Usage: dotnet run filename.fr\nTry running yourfile.fr instead."
        exit 1

    let content = File.ReadAllText (argv.[0])
    let body = go content
    let success = "Success! check frame.html"

    if (body <> "Invalid Expression")
    then buildHTML body |> ignore 
   
    if body = "Invalid Expression"
    then printfn "Invalid Expression"
    0