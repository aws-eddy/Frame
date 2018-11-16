module FrameParser

open Parser
open FrameInterpreter

type Expr = 
| HeadText of string
| Frame of Expr
//| MFrame of Expr * Expr

let expr, exprImpl = recparser()

// type Frame = 
// | (TextFrame * int)

// type Cols = int

//for variables
// type Context = Map<string, Value>

//parses the input to give an abstract syntax tree
// let parse e =
//     match e with
//     | TFrame t ->
//         match t with 
//         | HeadText(v) -> pbetween(pstr("HeadText(\""))(pstr(")"))(pstr(v))
//         | ParaText(v) -> pbetween(pstr("ParaText(\""))(pstr(")"))(pstr(v))
    // | Frame s -> 
    //     match s with
    //     | s ->  Expr    


//parses what type of TextFrame we have 
// let textFrame e = 
//     match e with 
//     | HeadText(v) -> "<h1>"+v+"</h1>"
//     | ParaText(v) -> "<h1>"+v+"</h1>"

// let rec eval e = 
//     match e with
//     | TFrame t -> textFrame t
//     | Frame(v) ->  "<div>" + eval e + "</div>"

let frame = pbetween (pstr "Frame(") (pchar ')') expr |>> (fun (e) -> Frame(e))

let inStr = pmany0 pletter |>> (fun v -> HeadText(stringify v))

let str = pbetween (pchar '"') (pchar '"') inStr

let tframe = pbetween (pstr "HeadText(") (pchar ')') str

exprImpl := tframe <|> frame

let grammar = pleft expr peof

let rec tab (s: string) (i: int) = 
 if i = 0
 then s
 else tab (s + "\t") (i-1)

let rec prettyprint (e:Expr) (i: int) : string= 
 match e with
 | HeadText(s) -> tab "" i + "<h1>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</h1>\n"
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 0
 | Failure ->  "Invalid Expression"
