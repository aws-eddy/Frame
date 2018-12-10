module FrameParser

open Parser
open FrameInterpreter
open System.Linq.Expressions
open System.Linq.Expressions
open FrameInterpreter
open FrameInterpreter

let mutable mapStatementChar = Map.empty

let expr, exprImpl = recparser()

// parse variable name after first character
let alphanum = pmany0 (pletter <|> pdigit)

//make sure first character of variable name is letter, then parse the rest
//and return it as a character list
let pvar = pseq pletter alphanum (fun(c, cl) -> (string c)+(stringify cl))

//parse the equal sign from the variable declaration
let vsuffix = pright (pstr "=") expr

//parse the variable name and expression and create a VariableDeclaration expression
let variabledeclaration = pseq pvar vsuffix (fun (str,e1) -> VariableDeclaration(str,e1))

//a parser for a variable being used in an expression NOT a variable declaration.
//NOTE: if input doesn't parse into any other expression type, it is assumed to be
//a variable. 
let variable = pvar |>> (fun e -> Variable(e))
//parses a <div>
let frame = pbetween ((pstr "Frame(") <|> (pstr "fr(")) (pchar ')') expr |>> (fun (e) -> Frame(e))

//parses a <nav>
let nav = pbetween ((pstr "NavFrame(") <|> (pstr "nav(")) (pchar ')') expr |>> (fun (e) -> NavFrame(e))

//parsers for input fields
let parseType = pbetween (pchar '\"') (pstr "\",") (pmany0 pstring |>> fun v -> stringify v)
let parseName = pbetween (pchar '\"') (pchar '\"') (pmany0 pstring |>> fun v -> stringify v)
let parseInputParams = pseq (parseType)(parseName) (fun (a,b)-> Input(a,b))
let input = pbetween ((pstr "Input(") <|> (pstr "in(")) (pchar ')') (parseInputParams)

//parsers for strings
let parseString =  pmany0 panything |>> (fun s -> String(stringify s))
//let testParseStringMulti = pseq (pright (pchar '"') (parseString)) (pchar '"') (fun (e1, e2) -> e1)
let testParseString = pbetween (pchar '"')(pchar '"') (parseString) |>> (fun (e1) -> e1)

//parses <b>
let bText = pbetween ((pstr "BoldText(") <|> (pstr "bt(")) (pchar ')') expr |>> (fun e -> BoldText(e))

//parses <h1>
let hText = pbetween ((pstr "HeadText(") <|> (pstr "ht(")) (pchar ')') expr |>> (fun e -> HeadText(e))

//parses <p>
let pText = pbetween ((pstr "ParaText(") <|> (pstr "pt(")) (pchar ')') expr |>> (fun e -> ParaText(e))

//parses link
let linkText = pbetween((pstr "LinkText(") <|> (pstr "lt(")) (pchar ')') expr |>> (fun e -> LinkText(e))

//parses <ol>
let olText = pbetween ((pstr "OrderedList(") <|> (pstr "ol(")) (pchar ')') expr |>> (fun e -> OrderedList(e))

//parses <li>
let liText = pbetween ((pstr "ListItem(") <|> (pstr "li(")) (pchar ')') expr |>> (fun e -> ListItem(e))

//parses buttons
let parseBtnText = pbetween (pchar '\"') (pstr "\",") (pmany0 pstring |>> fun t -> stringify t)

let mapColor s = 
    match s with
    | "blue" -> "primary"
    | "gray" -> "secondary"
    | "green" -> "success"
    | "red" -> "danger"
    | "yellow" -> "warning"
    | "teal" -> "info"
    | "white" -> "light"
    | "dark" -> "dark"
    | _ -> s    

let parseBtnColor = pbetween (pchar '\"') (pchar '\"') (pmany0 pstring |>> fun c -> mapColor(stringify c))

let parseBtnParams = pseq (parseBtnText) (parseBtnColor) (fun (t,c) -> Button(t,c))

let parseBtn = pbetween (pstr "btn(") (pchar ')') (parseBtnParams)

//parses list structure inside of expressions
let foff = pseq (pleft (hText <|> pText <|> frame <|> testParseString <|> variable) (pchar ',')) expr (fun (e1, e2) -> FofF(e1,e2))


exprImpl :=  foff <|> variabledeclaration <|> hText <|> bText <|> testParseString <|> pText <|> olText <|> liText <|> input <|> linkText <|> frame <|> nav <|> parseBtn <|> variable

let grammar = pleft expr peof

//characters which will always be indexed and replaced inside strings
//and do not need to be escaped
let specialIndexChars = ";$"
//characters removed when stripping statements of extra stuff
let ignoreChars = " \n\t"
//characters which will always be indexed and replaced inside strings
//but do need to be escaped
let indexChars = "\\;nt\""

/// <summary>Removes all spaces from string that aren't inside quotes.</summary>
/// <param name="str">A string representing an expression.</param>
/// <returns>A string with spaces removed.</returns>
let stripSpacesSelectively (str:string): string = 
    
    let sb = new System.Text.StringBuilder()
    
    let mutable b = false
    let mutable esc = false
    let mutable counter = 0

    for value in str do
        if value = '\n' && b then printfn "Illegal newline within string found!"
                                  exit 1
        else if (ignoreChars.Contains value) && not b then counter <- counter
        else if (indexChars.Contains value) && b && esc then esc <- not esc
                                                             if value = 'n' then mapStatementChar <- Map.add counter '\n' mapStatementChar
                                                             else if value = 't' then mapStatementChar <- Map.add counter '\t' mapStatementChar
                                                             else mapStatementChar <- Map.add counter value mapStatementChar
                                                             sb.Append ("$" + counter.ToString()+ "$") |> ignore
                                                             counter <- counter + 1
        else if esc then printfn "Illegal escape character found within string!"
                         exit 1
        else if value = '\\' && b then esc <- not esc
        else if (specialIndexChars.Contains value) && b then mapStatementChar <- Map.add counter value mapStatementChar
                                                             sb.Append ("$" + counter.ToString() + "$") |> ignore
                                                             counter <- counter + 1
        else if value = '"' then b <- not b
                                 sb.Append value |> ignore
        else sb.Append value |> ignore
    sb.ToString()

/// <summary>A function which splits a string by the string passed in.</summary>
/// <param name="s">A string.</param>
/// <returns>A list of strings.</returns>
let splitByString (s: string) = (fun (line : string) -> Seq.toList (line.Split s))

/// <summary>Removes characters from a string based on the stringed passed in.</summary>
/// <param name="str">A string.</param>
/// <returns>A string with characters removed.</returns>
let stripChars text (chars:string) =
    Array.fold (fun (s:string) c -> s.Replace(c.ToString(),"")) text (chars.ToCharArray())

/// <summary>Removes comments from input.</summary>
/// <param name="s">A string.</param>
/// <returns>A string.</returns>
let stripComments (s: string) = 

    let splitNewLine = splitByString "\n" s
    let result = new System.Text.StringBuilder()

    for str in splitNewLine do
        if not (str.Contains("//")) then result.Append (str + "\n") |> ignore
        else if not (str.StartsWith("//")) then let check2 = (splitByString "//" str).[0]
                                                result.Append (check2 + "\n") |> ignore
    result.ToString()

/// <summary>Generates the AST if an expression is parsed correctly.</summary>
/// <param name="s">A string.</param>
/// <returns>A string which is the HTML compiled code.</returns>
let getast (s: string) = 
    match grammar (prepare s) with
    | Success(e,_) -> eval e 1 "" ""
    | Failure -> "inv"

/// <summary>Prepares input for parsing.</summary>
/// <param name="input">A string.</param>
/// <returns>Returns a string which is all input compiled to HTML.</returns>
let parse input =

    let result = stripComments input

    let testEndChar = stripSpacesSelectively (result.ToString())

    //Make sure the last expression ends in a semicolon
    if (testEndChar.[testEndChar.Length-1]) <> ';' then printfn "Missing semicolon in last expression"
                                                        exit 1

    let splitSemiColon = splitByString ";" testEndChar

    let sb = new System.Text.StringBuilder()
    
    setMap mapStatementChar

    //go through each expression and add the output to the stringbuilder or exit if
    //an expression could not be parsed and compiled
    for value in splitSemiColon do
        if value <> "" then
            let res = (getast value)
            match res with
            | "inv" -> printfn "Invalid Syntax:\n%s\n\nEXITING" (value + ";")
                       exit 1
            | _ -> sb.Append res |> ignore
    
    sb.ToString()


 