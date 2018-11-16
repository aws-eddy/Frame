// Learn more about F# at http://fsharp.org

open System
open Parser
open LambdaParser
open AlphaReduction
let rec sub (v:char) (with_e:Expr) (in_e:Expr) =
 match in_e with
 | Application (e1, e2) -> let x = sub v with_e e1
                           let y = sub v with_e e2
                           Application(x, y)
 | Variable v' -> if v = v' 
                  then with_e
                  else Variable(v')
 | Abstraction(v', e') -> if v  = v' 
                          then (sub v with_e e')
                          else Abstraction(v', (sub v with_e e'))

let rec betastep (e: Expr) =
 match e with
 | Variable(v) -> e
 | Abstraction(v, e') -> Abstraction(v, (betastep e'))
 | Application(e1, e2) -> match e1 with
                          | Variable(v) -> Application(e1, (betastep e2))
                          | Abstraction(e1v, e1e) -> sub e1v e2 e1e
                          | Application(e11, e22) -> if not (e1 = betastep e1) 
                                                     then Application((betastep e1), e2)
                                                     else if not (e2 = betastep e2) 
                                                     then Application(e1, (betastep e2))
                                                     else e

let betanorm (e:Expr) =
 

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
