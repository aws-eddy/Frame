module AlphaReduction

open System
open Parser
open LambdaParser

(* prints out expression in original format *)
let rec lambdaprint(e: Expr) : string =
    match e with
    | Variable v -> v.ToString()
    | Abstraction(v,e') -> "(L" + v.ToString() + "." + (lambdaprint e') + ")"
    | Application(e1,e2) -> "(" + (lambdaprint e1) + (lambdaprint e2) + ")"

(* prints out sets *)
let setprint(xs: Set<'a>) : string =
    "{" + String.Join(", ", xs) + "}"

(* return the set of free variables
   in a lambda expression *)

let fv(e: Expr) : Set<char> =
    let rec fv'(e: Expr)(bindings: Set<char>) : Set<char> =
        match e with
        | Variable v ->
            if Set.contains v bindings then
                Set.empty
            else
                Set.add v Set.empty
        | Abstraction(v,e') ->
            fv' e' (Set.add v bindings)
        | Application(e1,e2) ->
            Set.union (fv' e1 bindings) (fv' e2 bindings)
    fv' e Set.empty

(* available variables *)
let letters = "abcdefghijklmnopqrstuvqxyz"

(* get a fresh variable *)
let freshvar(bindings: Set<char>) : char =
    let rec freshv i =
        match Set.contains letters.[i] bindings with
        | true  ->
            if (i + 1) = String.length letters then
                failwith "Expression is too long."
            freshv (i + 1)
        | false -> letters.[i]
    freshv 0

(* ensure that all bound variable names are
   unique, and not the same as any free variable *)
let rec alphanorm(e: Expr)(bindings: Set<char>)(renamings: Map<char,char>) : Expr*Set<char> =
    match e with
    | Variable v ->
        // renaming rule says that v should be renamed
        if Map.containsKey v renamings then
            // return an alpha-normalized variable & upd. bindings
            Variable(renamings.[v]), bindings
        // leave v as-is
        else
            // return what we started with
            e, bindings
    | Abstraction(v,e') ->
        // v is already bound in earlier expression
        if Set.contains v bindings then
            let v' = freshvar bindings
            let b' = Set.add v' bindings
            let r' = Map.add v v' renamings
            let (e'',b'') = alphanorm e' b' r'
            // return an alpha-normalized abstraction & upd. bindings
            Abstraction(v', e''), b''
        // we have never seen v before
        else
            let b' = Set.add v bindings
            let (e'',b'') = alphanorm e' b' renamings
            // return an alpha-normalized abstraction & upd. bindings
            Abstraction(v, e''), b''
    | Application(e1,e2) ->
        // alpha-normalize e1, then using the updated
        // bindings from e1, alpha-normalize e2
        let (e1',b')  = alphanorm e1 bindings renamings
        let (e2',b'') = alphanorm e2 b' renamings
        // return an alpha-normalized application & upd. bindings
        Application(e1', e2'), b''
