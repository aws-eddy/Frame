open System
open Parser
open LambdaParser
open AlphaReduction

/// <summary>Substitutes the expression with_e for v in the expression in_e.</summary>
/// <param name="v">Variable being substituted</param>
/// <param name="with_e">Expr being put in place of v</param>
/// <param name="in_e">Expr being changed</param>
/// <returns>An Expr</returns>
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

/// <summary>Applies one Expr to another Expr.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A one-step beta reduced Expr.</returns>
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

/// <summary>Alpha then Beta normalizes the Expr.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A reduced Expr.</returns>
let rec betanorm (e:Expr) =

 let x = (fst (alphanorm e (fv e) Map.empty))

 let rec normalize e1 =
  let e2 = betastep e1
  if (e2 = e1)
  then e2
  else (normalize e2)
 
 normalize x 

/// <summary>Parses input to an alpha and beta reduced Expr.</summary>
/// <param name="input">A string.</param>
/// <returns>A string with either the reduced expression or message indicating invalid expression.</returns>
let go input =
 match parse input with
 | Some e' -> lambdaprint (betanorm e')
 | None -> "Invalid Expression"

[<EntryPoint>]
let main argv =
 let input = argv.[0]
 printfn "%s" (go input)
 0