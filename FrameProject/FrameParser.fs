module FrameParser

open Parser

type Variable = string

type Expr = 
| ParaText of string
| HeadText of string
| ListItem of string
| LinkText of string
| Button of string
| AssignOp of Variable * Expr
| ListText of Expr
| OrderedList of Expr
| UnorderedList of Expr
| NavFrame of Expr
| Frame of Expr
| FofF of Expr * Expr

let ctx = Map.empty<string, Expr>

let expr, exprImpl = recparser()


let inStr c=
 match c with 
 | "p" -> pmany0 pstring  |>> ( fun v -> ParaText(stringify v))
 | "h" -> pmany0 pstring  |>> (fun v -> HeadText(stringify v))
 | "li" -> pmany0 pstring |>> (fun v -> ListItem(stringify v))
 | "link" -> pmany0 pstring |>> (fun v -> LinkText(stringify v))
 | "button" -> pmany0 pstring |>> (fun v -> Button(stringify v))
 | _ -> failwith "Type not defined!"    

// Assigning variables
let plet = pstr "let "
let alphanum = pmany0 (pletter <|> pdigit)
let pvar = pseq pletter alphanum (fun(c, cl) -> (string c)+(stringify cl))
let vprefix: Parser<Variable> = pright plet pvar
let vsuffix = pright (pstr " = ") expr

let value = pseq vprefix vsuffix (fun (vbl,e) -> AssignOp(vbl, e))

//handling a list of text Syntax: lst("h1", "str1", str2, ..., "str n") => <h1>str1</h1><h1>str2</h1> etc...
let betweenq (s:string) = pbetween (pchar '"') (pchar '"') (inStr s)
let makeAST s = 
    match s with 
    | "h1" -> HeadText(s)
    | _ -> failwith "Error: invalid tag specified in list construct"
let isTag  =  pseq ptag (psolo pdigit) (fun (a,b) -> makeAST (a.ToString() + (stringify b))) 
let extractTag = pseq (pchar '"') (pchar ',') (fun (tag, list) -> isTag 
let listText = pbetween ((pstr "ListText(") <|> (pstr "lst(")) (pchar ')') extractTag
let frame = pbetween ((pstr "Frame(") <|> (pstr "fr(")) (pchar ')') expr |>> (fun (e) -> Frame(e))
let nav = pbetween ((pstr "NavFrame(") <|> (pstr "nav(")) (pchar ')') expr |>> (fun (e) -> NavFrame(e))
let olText = pbetween ((pstr "OrderedText(") <|> (pstr "ol(")) (pchar ')') expr |>> (fun (e) -> OrderedList(e))
let ulText = pbetween ((pstr "UnorderedText(") <|> (pstr "ul(")) (pchar ')') expr |>> (fun (e) -> UnorderedList(e))
let hText = pbetween ((pstr "HeadText(") <|> (pstr "ht(")) (pchar ')') (betweenq "h")
let pText = pbetween ((pstr "ParaText(") <|> (pstr "pt(")) (pchar ')') (betweenq "p")
let liText = pbetween ((pstr "ListItem(") <|> (pstr "li(")) (pchar ')') (betweenq "li")
let linkText = pbetween((pstr "LinkText(") <|> (pstr "link(")) (pchar ')') (betweenq "link")
let parseBtn = pbetween (pstr "Button(") (pchar ')') (betweenq "button")

let ws = pright pws1 expr
let foff = pseq (pleft (hText <|> pText <|> liText <|> nav <|> frame) (pchar ',')) expr (fun (e1, e2) -> FofF(e1,e2))

exprImpl := ws <|> foff <|> frame <|> nav <|> listText <|> olText <|> ulText <|> hText <|> pText <|> liText <|> linkText <|> parseBtn

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
 | ListItem(s) -> wrap (s,"li") i
 | Button (s) -> wrap (s, "button") i
 | LinkText(s) -> link (s,"li") i
 | ListText(f) ->  tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"
 | UnorderedList (f) -> tab "" i + "<ul>\n" + (prettyprint f (i+1)) + tab "" i + "</ul>\n"
 | OrderedList (f) -> tab "" i + "<ol>\n" + (prettyprint f (i+1)) + tab "" i + "</ol>\n"
 | NavFrame (f) -> tab "" i + "<nav>\n" + (prettyprint f (i+1)) + tab "" i + "</nav>\n"
 | Frame(f) -> tab "" i + "<div>\n" + (prettyprint f (i+1)) + tab "" i + "</div>\n"
 | FofF(e1, e2) -> (prettyprint e1 i) +  (prettyprint e2 i)
 | _ -> failwith "Error: Not printable."

let go input =
 match grammar (prepare input) with
 | Success(e,_) -> prettyprint e 1
 | Failure ->  "Invalid Expression"