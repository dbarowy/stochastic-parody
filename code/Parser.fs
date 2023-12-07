module Parser
open Combinator

type TranslationUnit = {translate : bool; word: string}


type Expr =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
// TODO think about songs with numbers & commas and stuff
// TODO comments


let pneed a b c = (pseq a b c) <|> (pseq b a c) 
let pword = (pmany1 pletter) |>> stringify

let pntword = pword |>> (fun (x) -> {translate = false; word = x})
let ptword = pseq (pchar '!') pword (fun (x, y) -> {translate = true; word = y})
let pbword = pntword <|> ptword
let pline = pseq (pmany0 (pleft (pbword) (pchar ' '))) (pleft pbword pnl) (fun (f, e) -> Line (f @ [e]))

let pkeywords = pleft (pright (pstr "<KEYWORDS> ") (pmany0 pword) |>> (fun x -> Keywords x)) pnl

let psentiment = pleft (pright (pstr "<SENTIMENT> ") (pword) |>> (fun x -> Sentiment x)) pnl

let pexpr = pseq (pseq psentiment pkeywords (fun (a, b) -> a::[b])) (pmany1 pline) (fun (a, b) -> a @ b)
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
