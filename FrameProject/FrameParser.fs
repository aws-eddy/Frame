module FrameParser

open Parser
open FrameInterpreter

type Expr = 
| ParaText of string
| HeadText of string
| ListText of string
| Frame of Expr

//| MFrame of Expr * Expr

let expr, exprImpl = recparser()

let frame = pbetween (pstr "Frame(") (pchar ')') expr |>> (fun (e) -> Frame(e))
let inStr c=
 match c with 
 | "p" -> pmany0 pstring  |>> (fun v -> ParaText(stringify v))
 | "h" -> pmany0 pletter  |>> (fun v -> HeadText(stringify v))
 | "li" -> pmany0 pletter |>> (fun v -> ListText(stringify v))
 | _ -> failwith "Type not defined!"
let betweenq (s:string) = pbetween (pchar '"') (pchar '"') (inStr s)
let hText = pbetween (pstr "HeadText(") (pchar ')') (betweenq "h")
let pText = pbetween (pstr "ParaText(") (pchar ')') (betweenq "p")
let liText = pbetween (pstr "ListText(") (pchar ')') (betweenq "li")

exprImpl := hText <|> pText <|> liText <|> frame

let grammar = pleft expr peof

let rec tab (s: string) (i: int) = 
 if i = 0
 then s
 else tab (s + "\t") (i-1)

let rec prettyprint (e:Expr) (i: int) : string= 
 match e with
 | ParaText(s) -> tab "" i + "<p>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</p>\n"
 | HeadText(s) -> tab "" i + "<h1>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</h1>\n"
 | ListText(s) -> tab "" i + "<li>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</li>\n"
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 0
 | Failure ->  "Invalid Expression"