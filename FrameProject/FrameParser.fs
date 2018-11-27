module FrameParser

open Parser
open FrameInterpreter

type Expr = 
| ParaText of string
| HeadText of string
| ListText of string
| Variable of string
| AssignOp of string * Expr
| Frame of Expr
| FofF of Expr * Expr


type Value =
| ValueHeadText of string
| ValueParaText of string
| ValueListText of string
| ValueFrame of Expr

type Context = Map<string, Value>

let expr, exprImpl = recparser()

let frame = pbetween (pstr "Frame(") (pchar ')') expr |>> (fun (e) -> Frame(e))

let inStr c=
 match c with 
 | "p" -> pmany0 pstring  |>> (fun v -> ParaText(stringify v))
 | "h" -> pmany0 pstring  |>> (fun v -> HeadText(stringify v))
 | "li" -> pmany0 pstring |>> (fun v -> ListText(stringify v))
 | _ -> failwith "Type not defined!"

let betweenq (s:string) = pbetween (pchar '"') (pchar '"') (inStr s)
let hText = pbetween (pstr "HeadText(") (pchar ')') (betweenq "h")
let pText = pbetween (pstr "ParaText(") (pchar ')') (betweenq "p")
let liText = pbetween (pstr "ListText(") (pchar ')') (betweenq "li")
let ws = pright pws1 expr

let foff = pseq (pbetween (pstr "Frame(") (pstr "),") expr) expr (fun (e1, e2) -> FofF(Frame(e1),e2))

exprImpl := ws <|> foff <|> hText <|> pText <|> liText <|> frame

let grammar = pleft expr peof

let rec tab (s: string) (i: int) = 
 if i = 0
 then s
 else tab (s + "\t") (i-1)

let wrap (s, tag) (i:int) : string =  tab "" i + "<"+tag+">"+"\n" + tab "" (i+1) + s + "\n" + tab "" i + "</"+tag+">"+"\n"

let rec prettyprint (e:Expr) (i: int) : string= 
 match e with
 | ParaText(s) -> wrap (s,"p") i
 | HeadText(s) -> wrap (s,"h1") i
 | ListText(s) -> wrap (s,"li") i
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"
 | FofF(e1, e2) -> (prettyprint e1 i) +  (prettyprint e2 i)
 | _ -> failwith "Error: Not printable."

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 1
 | Failure ->  "Invalid Expression"