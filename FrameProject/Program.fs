//The Frame Project 

open System
open System.IO
open Parser

type TextFrame = 
| HeadText of string
| ParaText of string
| ListText of string * string
| LinkText of string * string

type Expr = 
| TFrame of TextFrame
| Frame of Expr
| MFrame of Expr * Expr

// type Frame = 
// | (TextFrame * int)

// type Cols = int

//for variables
// type Context = Map<string, Value>

//parses the input to give an abstract syntax tree
let parse e =
    match e with
    | TFrame t ->
        match t with 
        | HeadText(v) -> pbetween(pstr("HeadText(\""))(pstr(")"))(pstr(v))
        | ParaText(v) -> pbetween(pstr("ParaText(\""))(pstr(")"))(pstr(v))
    // | Frame s -> 
    //     match s with
    //     | s ->  Expr    


//parses what type of TextFrame we have 
let textFrame e = 
    match e with 
    | HeadText(v) -> "<h1>"+v+"</h1>"
    | ParaText(v) -> "<h1>"+v+"</h1>"

let rec eval e = 
    match e with
    | TFrame t -> textFrame t
    | Frame(v) ->  "<div>" + eval e + "</div>"

// let rec prettyprint e:string = 

let go input =
     match parse (prepare input) with
     | Success(e,_) -> Some e
     | Failure -> None

[<EntryPoint>]
let main argv =

    if Array.length argv <> 1 then 
        printf "Usage: dotnet run filename.fr"
        exit 1

    
    0
    
    let head = "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/><link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css\" integrity=\"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm\" crossorigin=\"anonymous\">
<title>Frame.io</title></head>"

    printfn "%s"  (head+"<body>" + content.ToString() + "</body></html>")

    File.WriteAllText("body.html", (head+"<body>" + content.ToString() + "</body></html>"))
    
    0 // return an integer exit code
