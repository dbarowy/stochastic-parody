module Evaluator
open Parser
open System.IO
open Sentiments

open System.Collections

type EmphAndPOS = {Emph: string; POS: string}

// lets put everything in twice --> once with real rhyme match the other with [] rhyme match to match w things that dont need to rhyme
// type EmphPOSAndRhyme = {emph: string; pos: string; rhyme: string list}
// hashmaps are word -> EmphPOSAndRhyme

let MATCH_POS = true
let SEE_TRANSLATION = false
// type Entry = { word: string; emphAndPOS: EmphAndPOS; rhyme: string List}
// type Entry = { word: string; emphPOSAndRhyme: EmphPOSAndRhyme}

// type Entry = { word: string; emphAndPOS: EmphAndPOS; rhyme: string List}
type features = { emphAndPOS: EmphAndPOS; rhyme: string List}

let reuse = new Hashtable()

// run checker for if words are in dictionary before running evaluator
let readDict (preferredArr: string list) =
    let wordToFeature = new Hashtable()
    let emphToWord = new Hashtable()
    let commonEmphToWord = new Hashtable()
    let preferredEmphToWord = new Hashtable()


    let dict = File.ReadAllText "cmu_pos_dict.txt"
    let common = File.ReadAllText "common.txt"


    let dictArr = dict.Split('\n')
    let commonArr = common.Split('\n')


    let dummy = dictArr |> Array.map (fun i -> 
        let spl: string list = i.Split(' ') |> Array.toList
        let len = (List.length spl)
        let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
        let myPOS = spl[1]
        let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
        let myEmphAndPOS = {Emph = myEmph; POS = myPOS}
        wordToFeature.Add(spl[0], box {emphAndPOS = myEmphAndPOS; rhyme = myRhyme})
        if emphToWord.ContainsKey(myEmphAndPOS) then
           let old: string list= emphToWord[myEmphAndPOS] |> unbox
           emphToWord.Remove(myEmphAndPOS)
           emphToWord.Add(myEmphAndPOS, box (spl[0]::old))

        else
            emphToWord.Add(myEmphAndPOS, box [spl[0]])
        
        )

    let dummy = commonArr |> Array.map (fun i ->
        let feat: features = wordToFeature[(i.ToUpper())] |> unbox
        let myEmph = feat.emphAndPOS
        if commonEmphToWord.ContainsKey(myEmph) then
           let old: string list = commonEmphToWord[myEmph] |> unbox
           commonEmphToWord.Remove(myEmph)
           commonEmphToWord.Add(myEmph, box (i::old))

        else
            commonEmphToWord.Add(myEmph, box [i])
    
        )

    let dummy = preferredArr |> List.map (fun i ->
        if wordToFeature.ContainsKey((i.ToUpper())) then
            let feat: features = wordToFeature[(i.ToUpper())] |> unbox
            let myEmph = feat.emphAndPOS
            if preferredEmphToWord.ContainsKey(myEmph) then

                let old: string list = preferredEmphToWord[myEmph] |> unbox
                preferredEmphToWord.Remove(myEmph)
                preferredEmphToWord.Add(myEmph, box (i::old))

            else
                preferredEmphToWord.Add(myEmph, box [i])
            
        )
    
        
    (wordToFeature, emphToWord, commonEmphToWord, preferredEmphToWord)

let evalSentiment sent = 
    if sent = "happy" then happy_sentiment
    else if sent = "depressing" then depressing_sentiment
    else []


let evalKeywords kws = 
    kws

// TODO: add common words
// TODO: basically we take 
let convert (lines: TranslationUnit list) (wtf: Hashtable) (etw: Hashtable) (cetw: Hashtable) (petw: Hashtable)=

    let rnd = System.Random()
    let rec helpConvert (ls: TranslationUnit list) = 
        match (ls: TranslationUnit list) with 
        | [] -> []
        | x::xs -> 
                if x.translate = true then
                    if reuse.ContainsKey(x.word) then 
                        let reusedWord = reuse[x.word] |> unbox
                        reusedWord::(helpConvert xs)
                    else
                        if wtf.ContainsKey(x.word.ToUpper()) = false then 
                                printfn "Word %A is not contained in the CMU Dictionary" x.word
                                printfn "Exiting"
                                exit(-1)
                        let myWord: features = wtf[(x.word.ToUpper())] |> unbox // used to be len
                        printfn "%A" myWord.rhyme
                        let (myEmphAndPOS: EmphAndPOS) = myWord.emphAndPOS

                        let translator = (
                            if petw.ContainsKey(myEmphAndPOS) then
                                petw
                            else if cetw.ContainsKey(myEmphAndPOS) then
                                cetw
                            else
                                etw
                            )
                        let wordList: string list = (translator[myEmphAndPOS]) |> unbox
                        let len = wordList.Length

                        let newWord = wordList[rnd.Next() % (len)] 

                        let updatedWordList = wordList |> List.filter(fun x -> x <> newWord)
                        translator.Remove(myEmphAndPOS)
                        if updatedWordList.Length <> 0 then
                            translator.Add(myEmphAndPOS, updatedWordList)
                        reuse.Add(x.word, newWord)
                        if SEE_TRANSLATION then printfn "%A, %A, %A, %A" x.word newWord myEmphAndPOS.POS myEmphAndPOS.Emph
                        
                        newWord::(helpConvert xs)
                else 
                    x.word::(helpConvert xs)
    helpConvert lines

let evalProg (ps: Expr list) = 
    // TODO combine
    let wordList = 
        (match ps[0] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> evalKeywords x
        | Line x -> []) @
        match ps[1] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> evalKeywords x
        | Line x -> []

    printfn "Assembing Dictionary"
    let wtf, etw, cetw, petw = readDict wordList
    printfn "Dictionary Complete"
    
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
                (convert x wtf etw cetw petw) :: helper xs


    (helper ps[2..])


let rec prettyprint (p: string list list): string =
    match p with
    | [] -> ""
    | x::xs ->( x |> String.concat " ") + "\n" + (prettyprint xs)