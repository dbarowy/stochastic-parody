module Parser
open Combinator

type TranslationUnit = {translate : bool; rhyme: bool; word: string}
// note variables are mutable?
// TODO: make sure doesnt have weird ws bugs
// TODO: can map numbers to words 1-> one
// TODO: default to translate all?
// TODO: the cmu dict didnt get rid of numbers i.e. WORD(1) -> WORD1
// TODO: cant handle empty sentiment
// ! is translate no rhyme
// $ is translate and rhyme

let TRANSLATE_DEFAULT = false

//NOTE: line has to start at start of line
// NOTE: cant reference var until declared

type Expr =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
| Section_name of string
// | Section of string * TranslationUnit List List // or just line list
| Section of string * (Expr List)
// | Section of Section_name * Line // or just line list

// TODO think about songs with numbers & commas and stuff
// TODO comments


let expr, exprImpl = recparser()

let ppad x = pbetween pws0 x pws0

let pnl1 = pmany1 pnl
let pspace1 = pmany1 (pchar ' ')

let pspace0 =  pmany0 (pchar ' ')

let pignore = pspace1 |>> ignore <|> (pbetween pspace0 ((pchar ',') |>> ignore) pspace0)

let pcletter = pletter <|> (pchar ''')

let pneed a b c = (pseq a b c) <|> (pseq b a c) 
let pword = (pmany1 pcletter) |>> stringify |>> (fun x ->
    if x[x.Length-3..x.Length-1] = "in'" then
        x[0..x.Length-4] + "ing"
    else if x[x.Length-2..x.Length-1] = "'s" then
        x[0..x.Length-3]
    else
        x
    )
let pword_no_translate = pword |>> (fun (x) -> {translate = TRANSLATE_DEFAULT; word = x; rhyme = false})
let pword_translate = pseq (pchar '!') pword (fun (x, y) -> {translate = (TRANSLATE_DEFAULT = false); word = y; rhyme = false})

let pword_rhyme = pseq (pchar '$') pword (fun (x, y) -> {translate = (TRANSLATE_DEFAULT = false); word = y; rhyme = true})

let pword_translation_unit = pword_no_translate <|> pword_translate <|> pword_rhyme
let pline = pseq (pmany0 (pleft (pword_translation_unit) pignore)) (pleft pword_translation_unit pnl1) (fun (f, e) -> Line (f @ [e]))

let pkeywords = pleft ((pright (pstr "<KEYWORDS>") (pmany0 (pright pspace1 pword))) |>> (fun x -> Keywords x)) pnl1

let psentiment = pleft (pright (pstr "<SENTIMENT>") ((pright pspace1 pword) |>> (fun x -> Sentiment x))) pnl1

let psection_name = (pleft (pright (pchar '*') (pmany1 pletter)) pnl1) |>> stringify |>> Section_name
// let psection_declarating = pmany1 pletter |>> stringify


let psection = pleft (pseq 
                        (pmany1 pletter |>> stringify)
                        (pbetween 
                            (pleft (ppad (pchar '=')) (ppad (pchar '[')))
                            (pmany1 (pright pwsNoNL0 pline))
                            (pbetween pws0 (pchar ']') pwsNoNL0))
                        (fun (x, y) -> Section(x, y)))
                        pnl1



// exprImpl := variable <|> application <|> abstraction
let pAll = pseq (pseq psentiment pkeywords (fun (a, b) -> a::[b])) (pmany1 (psection_name <|> psection <|> pline)) (fun (a, b) -> a @ b)

exprImpl :=  pAll//pseq (pseq (pseq psentiment pkeywords (fun (a, b) -> a::[b])) psection (fun (a, b) -> b::a)) (pmany1 pline) (fun (a, b) -> a @ b)
let grammar = pleft expr peof

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
