module Evaluator
open Parser
open System.IO

type Entry = { word: string; emph: string; rhyme: string List}

// run checker for if words are in dictionary before running evaluator
let readDict =
    let dict = File.ReadAllText "cmu_dict"
    let dictArr = dict.Split('\n')
    // let formatArr: Entry array = Array.create (Array.length dictArr) {word = ""; emph = ""; rhyme = [""]}
    // TODO remove for loop


    let inds = [|0.. (Array.length dictArr) - 1|]

    let formatArray = inds |> Array.map (fun i -> 
        let spl: string list = dictArr[i].Split(' ') |> Array.toList
        let len = (List.length spl)
        let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
        let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
        {word = spl[0]; emph = myEmph; rhyme = myRhyme}
        )
    // for i in 0.. (Array.length dictArr) - 1 do
        
    formatArray

let evalSentiment sent = 
    if sent = "happy" then ["adore"; "apple"; "atop"] else []


let evalKeywords kws = 
    kws

let rec searchWord n (word: string) (cmuDict: Entry array) = 

    match n with
    | -1 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    | i -> 
        if (cmuDict[i].word = word.ToUpper()) then cmuDict[i] else searchWord (n - 1)  word cmuDict 

// TODO make this be a mod circle
let rec searchEmph emph n (word: string) (cmuDict: Entry array) = 
    match n with
    | -1 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    | i -> if cmuDict[i].emph = emph && cmuDict[i].word <> word.ToUpper() then cmuDict[i] else searchEmph emph (n - 1) word cmuDict 

// TODO: add common words
// TODO: basically we take 
let convert (lines: string list) cmuDict =
    let len = ((Array.length cmuDict) - 1)

    let rnd = System.Random()
    let rec helpConvert ls = 
        match ls with 
        | [] -> []
        | x::xs -> 
            let myWord: Entry = searchWord (len) x cmuDict // used to be len
            let myEmph = myWord.emph
            let newWord = (searchEmph myEmph ((rnd.Next() % len)) x cmuDict).word // (rnd.Next() % len)
            newWord::(helpConvert xs)
    helpConvert lines

let evalProg (ps: Expr list) = 
    // TODO combine
    let cmuDict = readDict
    let wordList = 
        (match ps[0] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> evalKeywords x
        | Line x -> []) @
        match ps[1] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> evalKeywords x
        | Line x -> []

        
    let rec helper (ls: Expr list) = 
        match ls with
        | [] -> []
        | x::xs ->
            match x with
            | Sentiment y -> 
                helper xs

            | Keywords y -> 
                helper xs

            | Line x ->
                (convert x cmuDict) :: helper xs


    (helper ps[2..])


        

let rec prettyprint (p: string list list): string=
    match p with
    | [] -> ""
    | x::xs ->( x |> String.concat " ") + "\n" + (prettyprint xs)