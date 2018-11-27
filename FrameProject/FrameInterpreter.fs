module FrameInterpreter

open System.IO
let buildHTML body:string = 

    let html = "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css\" integrity=\"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm\" crossorigin=\"anonymous\">
     <title>Frame.io</title>\n</head>\n" + "<body>\n" + body + "</body>\n</html>"

    File.WriteAllText("frame.html", html)
    html
    
    