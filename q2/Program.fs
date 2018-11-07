module LambdaParser

open Parser

type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr

/// <summary>Turns an AST into a string for your reading pleasure.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A string.</returns>
let rec prettyprint e =
 match e with
 | Variable(v) -> "Variable(" + v.ToString() + ")"
 | Abstraction(v,e') -> "Abstraction(Variable(" + v.ToString() + "), " + (prettyprint e') + ")"
 | Application(e1,e2) -> "Application(" + (prettyprint e1) + ", " + (prettyprint e2) + ")"

/// <summary>Makes the AST a string in the form in which it was input originally.</summary>
/// <param name="e">Expression being made into string.</param>
/// <returns>A string representing the AST.</returns>
let rec lambdaprint (e : Expr) : string =
 match e with
 | Variable(v) -> v.ToString()
 | Abstraction(v,e') -> "(L" + v.ToString() + "." + lambdaprint e' + ")"
 | Application(e1,e2) -> "(" + (lambdaprint e1) + (lambdaprint e2) + ")"

/// <summary>Returns a set of all free variables in the expression.</summary>
/// <param name="e">And Expr.</param>
/// <returns>A set of all free variables in the expression.</returns>
let rec fv (e:Expr) : Set<char> =
 //printfn "%A" e
 match e with
 | Variable(v) -> set [v]
 | Abstraction(v,e') -> match (Set.contains v (fv e')) with
                        | true -> //printfn "Set contains %c" v
                                  Set.remove v (fv e')
                        | false -> (fv e')
 | Application(e1,e2) -> Set.union (fv e1) (fv e2)

/// <summary>Returns the next alphabet character which is not a free variable.</summary>
/// <param name="b">Set of characters.</param>
/// <returns>The next character in the alphabet not in the input set.</returns>
let freshvar (b:Set<char>) : char =
 let x = Set.difference (set ['a' .. 'z']) b
 Set.minElement x
 
/// <summary>Alpha reduces the given expression.</summary>
/// <param name="e">The Expr being alpha reduced.</param>
/// <param name="b">A Set<char> containing variables in use in expression.</param>
/// <param name="r">A Map<char,char> containing remappings for alpha reduced variables.</param>
/// <returns>A tuple of the alpha reduced expression and a set of characters in use in the expression.</returns>
let rec alphanorm (e: Expr) (b:Set<char>) (r:Map<char,char>) : Expr * Set<char> =
 match e with
 | Variable(v) -> if (Map.containsKey v r) then
                   (Variable((Map.find v r)), b)
                  else
                   (Variable(v), b)
 | Abstraction(v,e') -> match (Set.contains v b) with
                        | true -> let v' = (freshvar b)
                                  let b' = (Set.add v' b)
                                  (Abstraction(v', (fst (alphanorm e' b' (Map.add v v' r)))), b')
                        | false -> let b' = (Set.add v b)
                                   (Abstraction(v, (fst (alphanorm e' b' r))), b')
 | Application(e1,e2) -> let x = (alphanorm e1 b r)
                         let y = (alphanorm e2 (snd x) r)
                         (Application(fst x, fst y), snd y)

let expr, exprImpl = recparser()

/// <summary>Parses lambda variables.</summary>
/// <returns>A Variable value.</returns>
let variable = pletter |>> (fun v -> Variable(v))

/// <summary>Parses the inner guts of a lambda expression.</summary>
/// <returns>An abstraction value.</returns>
let lam = pseq (pleft pletter (pchar '.')) expr (fun (v,e) -> Abstraction(v,e))

/// <summary>Parses lambda abstractions.</summary>
/// <returns>An abstraction value.</returns>
let abstraction = pbetween (pstr "(L") (pchar ')') lam

/// <summary>Parses lambda applications.</summary>
/// <returns>An Abstraction value.</returns>
let application = pbetween (pchar '(') (pchar ')') (pseq expr expr (fun (e1,e2) -> Application(e1,e2)))

/// <summary>Parses an entire lambda expression.</summary>
/// <returns>An Expr value.</returns>
exprImpl := variable <|> abstraction <|> application

/// <summary>Parses an entire lambda expression, including EOF.</summary>
/// <returns>An Expr value.</returns>
let grammar = pleft expr peof

/// <summary>Returns Some Expr if the input conforms to the grammar, otherwise returns None.</summary>
/// <param name="input">The input string.</param>
/// <returns>An Expr option value.</returns>
let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None

/// <summary>Returns a string that is either the alpha reduced expression or a message indicating the expression is invalid.</summary>
/// <param name="input">The input string.</param>
/// <returns>A string.</returns>
let itgo input : string =
 match parse input with
 | Some ast -> lambdaprint (fst (alphanorm ast (fv ast) Map.empty))
 | None -> "Invalid Expression"
 
[<EntryPoint>]
let main argv =

 let input = argv.[0]

 printfn "%s" (itgo input)

 0