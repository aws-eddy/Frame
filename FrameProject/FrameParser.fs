module FrameParser

open Parser
open FrameInterpreter

type Expr = 
| ParaText of string
| HeadText of string
| ListText of string
| LinkText of string
| Button of string
| NavFrame of Expr
| Variable of string
| Frame of Expr
| FofF of Expr * Expr


type Value =
| ValueText of string
| ValueFrame of Expr

type Context = Map<string, Expr>

let expr, exprImpl = recparser()
let value, valueImpl = recparser()

let inStr c=
 match c with 
 | "p" -> pmany0 pstring  |>> ( fun v -> ParaText(stringify v))
 | "h" -> pmany0 pstring  |>> (fun v -> HeadText(stringify v))
 | "li" -> pmany0 pstring |>> (fun v -> ListText(stringify v))
 | "link" -> pmany0 pstring |>> (fun v -> LinkText(stringify v))
 | "button" -> pmany0 pstring |>> (fun v -> Button(stringify v))
 | _ -> failwith "Type not defined!"    

// Assigning variables
let plet = pstr "let "
let alphanum = pmany0 (pletter <|> pdigit)
let pvar = pseq pletter alphanum (fun(c, cl) -> (string c)+(stringify cl))
let vprefix = pright plet pvar
let vsuffix = pright (pstr " = ") expr

let valuetext = 

let valueFrame

let value = valuetext <|> valueframe

// let assign (name:string,value:Expr ) ctx = 
//   match value with 
//   | HeadText(s) -> Map.add name value ctx
//   | _ -> failwith "Cannot assign variable: invald expression"

//*
//let variable : Input -> Map<string,Expr> = pseq vprefix vsuffix (fun (name,v) -> Map.add(name,v))
//



let frame = pbetween ((pstr "Frame(") <|> (pstr "fr(")) (pchar ')') expr |>> (fun (e) -> Frame(e))
let nav = pbetween ((pstr "NavFrame(") <|> (pstr "nav(")) (pchar ')') expr |>> (fun (e) -> NavFrame(e))
let betweenq (s:string) = pbetween (pchar '"') (pchar '"') (inStr s)
let hText = pbetween ((pstr "HeadText(") <|> (pstr "ht(")) (pchar ')') (betweenq "h")
let pText = pbetween ((pstr "ParaText(") <|> (pstr "pt(")) (pchar ')') (betweenq "p")
let liText = pbetween ((pstr "ListText(") <|> (pstr "lst(")) (pchar ')') (betweenq "li")

let linkText = pbetween((pstr "LinkText(") <|> (pstr "link(")) (pchar ')') (betweenq "link")

let parseBtn = pbetween (pstr "Button(") (pchar ')') (betweenq "button")

// let button  = pbetween ((pstr "Button(")<|> (pstr("btn")) (pchar ')') parseBtn

let ws = pright pws1 expr
let foff = pseq (pleft (hText <|> pText <|> liText <|> frame) (pchar ',')) expr (fun (e1, e2) -> FofF(e1,e2))

exprImpl := ws <|> foff <|> hText <|> pText <|> liText <|> linkText <|> frame <|> nav <|> parseBtn

let grammar = pleft expr peof

let rec tab (s: string) (i: int) = 
 if i = 0
 then s
 else tab (s + "\t") (i-1)

let wrap (s, tag) (i:int) : string =  tab "" i + "<"+tag+">"+"\n" + tab "" (i+1) + s + "\n" + tab "" i + "</"+tag+">"+"\n"
let link (s,tag) (i:int) :string = tab "" i + "<a href =\"/"+s+"\"><"+tag+">"+"\n" + tab "" (i+1) + s + "\n" + tab "" i + "</"+tag+"></a>"+"\n"

let rec prettyprint (e:Expr) (i: int) : string= 
 match e with
 | ParaText(s) -> wrap (s,"p") i
 | HeadText(s) -> wrap (s,"h1") i
 | ListText(s) -> wrap (s,"li") i
 | Button (s) -> wrap (s, "button") i
 | LinkText(s) -> link (s,"li") i
 | NavFrame (f) -> tab "" i + "<nav>\n" + (prettyprint f (i+1)) + tab "" i + "</nav>\n"
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"
 | FofF(e1, e2) -> (prettyprint e1 i) +  (prettyprint e2 i)
 | _ -> failwith "Error: Not printable."

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 1
 | Failure ->  "Invalid Expression"