module Parser
open Combinator

type TranslationUnit = {word: string; translate : bool; rhyme: bool}
// TODO: make sure doesnt have weird ws bugs
// TODO: can map numbers to words 1-> one
// TODO: default to translate all?

let TRANSLATE_DEFAULT = false

// NOTE: line has to start at start of line
// NOTE: cant reference var until declared
// NOTE: do not leave keywords or sentiment blank
// NOTE: have to have new line char before closing bracket of section
// NOTE: ! is translate no rhyme
// NOTE: $ is translate and rhyme
// NOTE: variables are mutable
// NOTE: have to have sentiment before keywords if have both --> hard to change if we let them be empty bc it will parse A as empty first
// NOTE: everything is separated by 1 or more newline characters


type Expr =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
| Section_Instance of string
| Section of string * (Expr List)

// Initialize parser
let expr, exprImpl = recparser()

// parser to allow ws around a given parser
let ppad x = pbetween pws0 x pws0

// parser for 1 or more newline characters
let pnl1 = pmany1 pnl

// parser for 1 or more spaces characters
let pspace1 = pmany1 (pchar ' ')
// parser for 0 or more spaces characters
let pspace0 =  pmany0 (pchar ' ')

// parser for ignoring white space and commas between word
let pignore = (pbetween pspace0 ((pchar ',') |>> ignore) pspace0) <|> (pspace1 |>> ignore)

// parser for a letter or a apostrophy
let pcletter = pletter <|> (pchar ''')

// parser for a word: parsers a word as a series of letters and apostrophies
// also has corrections for common abreviation in' -> ing
let pword = (pmany1 pcletter) |>> stringify |>> (fun x ->
    if x[x.Length-3..x.Length-1] = "in'" then
        x[0..x.Length-4] + "ing"
    else
        x
    )
// parser for a word that is not supposed to be translated
let pword_no_translate = pword |>> (fun (x) -> {translate = TRANSLATE_DEFAULT; word = x; rhyme = false})
// parser for a word that is should be translated but not rhymed denoted by ! before the word
let pword_translate = pseq (pchar '!') pword (fun (x, y) -> {translate = (TRANSLATE_DEFAULT = false); word = y; rhyme = false})
// parser for a word that is should be translated and rhymed denoted by $ before the word
let pword_rhyme = pseq (pchar '$') pword (fun (x, y) -> {translate = (TRANSLATE_DEFAULT = false); word = y; rhyme = true})
// parser to parse any variation of word: no translate, translate, or rhyme
let pword_translation_unit = pword_no_translate <|> pword_translate <|> pword_rhyme

// parser to parse a whole line of words of any variation and agregate them into a line
let pline = pseq (pmany0 (pleft (pword_translation_unit) pignore)) (pleft pword_translation_unit pnl1) (fun (f, e) -> Line (f @ [e]))

// parser to parse the list of keywords, keywords can be separated by any number of spaces and at most 1 comma
// this is an optional field
let pkeywords = (pleft ((pright (pstr "<KEYWORDS>") (pmany0 (pright pignore pword))) |>> Keywords) pnl1) <|> (pws0 |>> (fun x -> Keywords []))

// parser to parse the sentiment, this is an optional field
let psentiment = (pleft (pright (pstr "<SENTIMENT>") (((pright pspace1 pword))  |>> Sentiment)) pnl1) <|> (pws0 |>> (fun x -> Sentiment ""))

// parser for an instance of a section, a section reference should be preceeded with the * character
let psection_name = (pleft (pright (pchar '*') (pmany1 pletter)) pnl1) |>> stringify |>> Section_Instance

// parser for declaring a section: a section declarating is of the following form:
// section = [
//   ... 
// ]
// the newline before the close bracket is essential
let psection = pleft (pseq 
                        (pmany1 pletter |>> stringify)
                        (pbetween 
                            (pleft (ppad (pchar '=')) (ppad (pchar '[')))
                            (pmany1 (pright pwsNoNL0 pline))
                            (pbetween pws0 (pchar ']') pwsNoNL0))
                        (fun (x, y) -> Section(x, y)))
                        pnl1

// parser for the implementation of an expression
// it techincally requires a sentiment and keywords as the first 2 expressions,
// but those parser can parse nothing so they aren't really necesary
// then it needs one or more line or section to be considered a song
// all inputs also must end with a newline
exprImpl :=  pseq (pseq psentiment pkeywords (fun (a, b) -> a::[b])) (pmany1 (psection_name <|> psection <|> pline)) (fun (a, b) -> a @ b)

// parser for the whole grammar
// accepts as many newline characters as wanted before first expression
// parses an expression as defined above until the end of the file
let grammar = pright (pmany0 pnl) (pleft expr peof)

// driver for parsing a given input
// @param s: a string that is a .song file
// prints error message if parse fails
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
