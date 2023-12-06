module Evaluator
open Parser
open System.IO

open System.Collections

type Entry = { word: string; emph: string; rhyme: string List}
type features = { emph: string; rhyme: string List}


// run checker for if words are in dictionary before running evaluator
let readDict =
    let wordToFeature = new Hashtable();
    let emphToWord = new Hashtable();
    let commonEmphToWord = new Hashtable();


    let dict = File.ReadAllText "cmu_dict"
    let common = File.ReadAllText "common.txt"

    let dictArr = dict.Split('\n')
    let commonArr = common.Split('\n')

    // let cinds = [|0.. (Array.length commonArr) - 1|]
    
    // let formatArr: Entry array = Array.create (Array.length dictArr) {word = ""; emph = ""; rhyme = [""]}

    let inds = [|0.. (Array.length dictArr) - 1|]

    let formatArray = inds |> Array.map (fun i -> 
        let spl: string list = dictArr[i].Split(' ') |> Array.toList
        let len = (List.length spl)
        let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
        let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
        wordToFeature.Add(spl[0], box {emph = myEmph; rhyme = myRhyme})
        if emphToWord.ContainsKey(myEmph) then
            //  (emphToWord[myEmph] = spl[0]::(emphToWord[myEmph]))
           let old: string list= emphToWord[myEmph] |> unbox
           emphToWord.Remove(myEmph)
           emphToWord.Add(myEmph, box (spl[0]::old))

        else
            emphToWord.Add(myEmph, box [spl[0]])
        
        )
    // for i in 0.. (Array.length dictArr) - 1 do

    let dummy = commonArr |> Array.map (fun i ->
        let feat: features = wordToFeature[(i.ToUpper())] |> unbox
        let myEmph = feat.emph
        if commonEmphToWord.ContainsKey(myEmph) then
            //  (emphToWord[myEmph] = spl[0]::(emphToWord[myEmph]))
           let old: string list = commonEmphToWord[myEmph] |> unbox
           commonEmphToWord.Remove(myEmph)
           commonEmphToWord.Add(myEmph, box (i::old))

        else
            commonEmphToWord.Add(myEmph, box [i])
        
        // commonEmphToWord.Add(i, emphToWord[i])
    
        )
        
    (wordToFeature, emphToWord, commonEmphToWord)

let evalSentiment sent = 
    if sent = "happy" then ["adore"; "apple"; "atop"] else []


let evalKeywords kws = 
    kws

// let rec searchWord n (word: string) (cmuDict: Entry array) = 

//     match n with
//     | -1 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
//     | i -> 
//         if (cmuDict[i].word = word.ToUpper()) then cmuDict[i] else searchWord (n - 1)  word cmuDict 

// // TODO make this be a mod circle
// let rec searchEmph emph n (word: string) (cmuDict: Entry array) = 
//     match n with
//     | -1 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
//     | i -> if cmuDict[i].emph = emph && cmuDict[i].word <> word.ToUpper() then cmuDict[i] else searchEmph emph (n - 1) word cmuDict 

// TODO: add common words
// TODO: basically we take 
let convert (lines: string list) (wtf: Hashtable) (etw: Hashtable) (cetw: Hashtable) =
    // let len = ((Array.length cmuDict) - 1)

    let rnd = System.Random()
    let rec helpConvert ls = 
        match (ls: string list) with 
        | [] -> []
        | x::xs -> 
            let myWord: features = wtf[(x.ToUpper())] |> unbox // used to be len
            let (myEmph: string) = myWord.emph
            // printfn "%A" myWord
            let translator = if cetw.ContainsKey(myEmph) then cetw else etw
            let wordList: string list = (translator[myEmph]) |> unbox
            let len = wordList.Length
            // printfn "%A" (etw.ContainsKey(myEmph))
            // printfn "%A" (wordList[0])
            let newWord = wordList[rnd.Next() % (len - 1)] //rnd.Next() % len

            newWord::(helpConvert xs)
    helpConvert lines

let evalProg (ps: Expr list) = 
    // TODO combine
    let wtf, etw, cetw = readDict
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
                (convert x wtf etw cetw) :: helper xs


    (helper ps[2..])


        

let rec prettyprint (p: string list list): string=
    match p with
    | [] -> ""
    | x::xs ->( x |> String.concat " ") + "\n" + (prettyprint xs)