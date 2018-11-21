module FrameParser

open Parser
open FrameInterpreter

type Expr = 
| ParaText of string
| HeadText of string
| Frame of Expr
//| MFrame of Expr * Expr

let expr, exprImpl = recparser()

let frame = pbetween (pstr "Frame(") (pchar ')') expr |>> (fun (e) -> Frame(e))

let inStr = pmany0 pletter |>> (fun v -> HeadText(stringify v))
let hstr = pbetween (pchar '"') (pchar '"') inStr
let hText = pbetween (pstr "HeadText(") (pchar ')') hstr

let inStr2 =  pmany0 pletter |>> (fun v -> ParaText(stringify v))
let str2 = pbetween (pchar '"') (pchar '"') inStr2
let pText = pbetween (pstr "ParaText(") (pchar ')') str2

exprImpl := hText <|> pText <|> frame

let grammar = pleft expr peof

let rec tab (s: string) (i: int) = 
 if i = 0
 then s
 else tab (s + "\t") (i-1)

let rec prettyprint (e:Expr) (i: int) : string= 
 match e with
 | ParaText(s) -> tab "" i + "<p>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</p>\n"
 | HeadText(s) -> tab "" i + "<h1>\n" + tab "" (i+1) + s + "\n" + tab "" i + "</h1>\n"
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 0
 | Failure ->  "Invalid Expression"