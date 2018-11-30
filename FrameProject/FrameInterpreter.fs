module FrameInterpreter
open FrameParser
open System.IO
open FrameParser
let buildHTML body:string = 

    let html = "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css\" integrity=\"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm\" crossorigin=\"anonymous\">
     <title>Frame.io</title>\n</head>\n" + "<body>\n" + body + "</body>\n</html>"

    File.WriteAllText("frame.html", html)
    html
    
let rec eval (e:Expr) (ctx:Map<Variable, Expr>) = 
     
     match e with 
     | AssignOp (var,e) ->
          let (e', ctx1) = eval e ctx     
          match e' with
          | ParaText s -> Map.add var e' ctx
          e',ctx1
          | HeadText s -> Map.add var e' ctx
          e',ctx1
          | ListText s -> Map.add var e' ctx
          e',ctx1
          | ParaText s -> Map.add var e' ctx
          e',ctx1
          | LinkText s -> Map.add var e' ctx
          e',ctx1
          | Button s -> Map.add var e' ctx
          e',ctx1
          | _ -> failwith "Operation not defined"