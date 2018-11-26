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


type Value =
| ValueHeadText of string
| ValueParaText of string
| ValueListText of string
| ValueFrame of Expr

type Context = Map<string, Value>

//| MFrame of Expr * Expr

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



// let rec eval e ctx : Expr*Context = 
//     match e with 
//     | Variable v -> 
//         match ctx.[v] with
//         | ValueParaText s -> ParaText s, ctx
//         | ValueHeadText s -> HeadText s,  ctx
//         | ValueListText s  -> ListText s, ctx
//         | ValueFrame e ->  Frame e, ctx
//     | AssignOp (s, e') ->
//         let (e'', ctx1) = eval e' ctx
//         match e'' with
//         | HeadText h -> 
//             let ctx1 = Map.add h (HeadText h) ctx
//             e'', ctx1
//         |ParaText p ->
//             let ctx1 = Map.add p (ParaText p) ctx
//             e'', ctx1
//         |ListText li ->
//             let ctx1 = Map.add li (ListText li) ctx
//             e'', ctx1             



exprImpl := hText <|> pText <|> liText <|> frame

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
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>"
 | _ -> failwith "Error: Not printable."

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 1
 | Failure ->  "Invalid Expression"