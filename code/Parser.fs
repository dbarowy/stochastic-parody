module Parser
open Combinator

type TranslationUnit = {translate : bool; rhyme: bool; word: string}

// TODO: can tell it to maintain rhyme??
// TODO: can map numbers to words 1-> one

// ! is translate no rhyme
// $ is translate and rhyme

type Expr =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
// TODO think about songs with numbers & commas and stuff
// TODO comments

let pspace1 = pmany1 (pchar ' ')

let pspace0 =  pmany0 (pchar ' ')

let pignore = pspace1 |>> ignore <|> (pbetween pspace0 ((pchar ',') |>> ignore) pspace0)

let pcletter = pletter <|> (pchar ''')

let pneed a b c = (pseq a b c) <|> (pseq b a c) 
let pword = (pmany1 pcletter) |>> stringify |>> (fun x -> if x[x.Length-3..x.Length-1] = "in'" then x[0..x.Length-4] + "ing" else x)
let pword_no_translate = pword |>> (fun (x) -> {translate = false; word = x; rhyme = false})
let pword_translate = pseq (pchar '!') pword (fun (x, y) -> {translate = true; word = y; rhyme = false})

let pword_rhyme = pseq (pchar '$') pword (fun (x, y) -> {translate = true; word = y; rhyme = true})

let pword_translation_unit = pword_no_translate <|> pword_translate <|> pword_rhyme
let pline = pseq (pmany0 (pleft (pword_translation_unit) pignore)) (pleft pword_translation_unit pnl) (fun (f, e) -> Line (f @ [e]))

let pkeywords = pleft ((pright (pstr "<KEYWORDS>") (pmany0 (pright pspace1 pword))) |>> (fun x -> Keywords x)) pnl

let psentiment = pleft (pright (pstr "<SENTIMENT>") (pright pspace1 pword) |>> (fun x -> Sentiment x)) pnl

let pexpr = pseq (pseq psentiment pkeywords (fun (a, b) -> a::[b])) (pmany1 pline) (fun (a, b) -> a @ b)
let grammar = pleft pexpr peof

let parse s = 
    let i = prepare s
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(pos, rule) -> 
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at position %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos s msg
        printf "%s" diag
        None
