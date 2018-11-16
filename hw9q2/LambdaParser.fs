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
