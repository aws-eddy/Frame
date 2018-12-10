module FrameInterpreter

open System
open System.IO
open Parser

//map which holds strings to expressions for variables
let mutable ctx = Map.empty
//map which holds the symbols removed from input text so it can be properly parsed
let mutable mapStatementChar = Map.empty
let setMap (m: Map<int, char>) = 
     mapStatementChar <- m

/// <summary>Generates the output file frame.html</summary>
/// <param name="body">String being put inside header</param>
let buildHTML body= 

    let html = "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css\" integrity=\"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm\" crossorigin=\"anonymous\">
     <title>Frame.io</title>\n</head>\n" + "<body>" + body + "\n</body>\n</html>"

    File.WriteAllText("frame.html", html)

/// <summary>Adds tabs to the string passed in</summary>
/// <param name="s">String two which tabs are being added</param>
/// <param name="i">int representing number of tabs being added</param>
/// <returns>A string with i tabs added to front</returns>
let rec tab (s: string) (i: int) = 
     if i = 0
     then s
     else tab (s + "\t") (i-1)

/// <summary>Replaces special symbols with the characters originally typed</summary>
/// <param name="s">A string being checked for replacement symbols</param>
/// <returns>A string with all special symbols replaced</returns>
let replaceSymbols (s:string) (i: int) =
     
     //if string doesn't contain special symbol markers, it can just be returned
     if s.Contains "$" then
          
          let sb = new System.Text.StringBuilder()
          let mutable b = false
          let mutable strInt = ""

          //go through each character in string to build new string with special symbols
          //replaced
          for v in s do
               
               if v = '$' && b then b <- false
                                    sb.Append (mapStatementChar.[(strInt|>int)]) |> ignore
                                    if (mapStatementChar.[(strInt|>int)] = '\n') then sb.Append (tab "" i)|> ignore                                    
                                    strInt <- ""
               else if v = '$' then b <- true
               else if b then strInt <- strInt + v.ToString()
               else sb.Append v |> ignore

          sb.ToString()

     else s

/// <summary>Compiles a link expression into HTML code</summary>
/// <param name="s">A string representing text within link</param>
/// <param name="tag">A string representing tag in which s is being wrapped</param>
/// <param name="i">current tab number</param>
/// <returns>A string representing the HTML compiled code</returns>
let link (s,tag) (i:int) :string = "\n<a href =\"/"+ s+"\"><"+tag+">"+"\n" + tab "" (i+1) + s + "\n" + tab "" i + "</"+tag+"></a>"

/// <summary>Converts expressions to HTML code recursively</summary>
/// <param name="e">An Expr being converted to HTML</param>
/// <param name="i">An int representing the number of tabs</param>
/// <param name="head">A string representing the top portion of the current wrap</param>
/// <param name="tail">A string representing the bottom portion of the current wrap</param>
/// <returns>A string representing the HTML compiled code</returns>
let rec eval (e:Expr) (i: int) (head: string) (tail:string): string= 
     match e with
     | String (s) -> head + "\n" + (tab "" i) + (replaceSymbols s i) + tail
     | BoldText (s) -> eval s (i+1) (head + "\n"+ (tab "" i) + "<b>") ("\n" + (tab "" i) + "</b>" + tail ) 
     | ParaText(s) -> eval s (i+1) (head + "\n" + (tab "" i) + "<p>") ("\n" + (tab "" i) + "</p>" + tail )
     | HeadText(s) -> eval s (i+1) (head + "\n" + (tab "" i) + "<h1>") ("\n" + (tab "" i) + "</h1>" + tail )
     | ListItem(s) -> eval s (i+1) (head + "\n" + (tab "" i) + "<li>") ("\n" + (tab "" i) + "</li>" + tail )
     | OrderedList(s) -> eval s (i+1) (head + "\n" + (tab "" i) + "<ol>") ("\n" + (tab "" i) + "</ol>" + tail )
     | Button (t,c) -> "\n" + tab "" i + "<button class = \"btn btn-" + c+"\">"+ t + "</button>"
     | LinkText(s) -> link (eval s i head tail,"li") i
     | Input(t, label) -> "\n" + tab "" i + "<label>"+label+" </label><br/>\n" + tab "" i + "<input type= " + t+" >" 
     | NavFrame (f) -> "\n" + tab "" i + "<nav>" + (eval f (i+1) head tail) + "\n"+ tab "" i + "</nav>"
     | Frame(f) -> "\n"+ tab "" (i) + "<div>" + (eval f (i+1) head tail) + "\n"+tab "" i + "</div>"
     | FofF(e1, e2) -> (eval e1 i head tail) + (eval e2 i head tail)
     | VariableDeclaration(s, e1) -> ctx <- Map.add s e1 ctx 
                                     ""
     | Variable(s) -> if (Map.containsKey s ctx) then eval (Map.find s ctx) i head tail 
                      else printfn "Variable %s is used but does not exist.\n\nEXITING" s
                           exit 1
