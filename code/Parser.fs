module Parser
open Combinator

type Expr =
| Sentiment of string
| Keyword of string List
| Line of string List
// TODO think about songs with numbers


let pneed a b c = (pseq a b c) <|> (pseq b a c) 
let pword = (pmany1 pletter) |>> stringify
let pline = pseq (pmany0 (pleft (pword) (pchar ' '))) (pleft pword pnl) (fun (f, e) -> Line (f @ [e]))

let pkeyword = pleft (pright (pstr "<KEYWORDS> ") (pmany0 pword) |>> (fun x -> Keyword x)) pnl

let psentiment = pleft (pright (pstr "<SENTIMENT> ") (pword) |>> (fun x -> Sentiment x)) pnl

let pexpr = pseq (pseq psentiment pkeyword (fun (a, b) -> a::[b])) (pmany1 pline) (fun (a, b) -> a @ b)

// let pexpr = pmany1 pline
let grammar = pleft pexpr peof

let parse s = 
    let i = debug s
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(pos, rule) -> 
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at position %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos s msg
        printf "%s" diag
        None
