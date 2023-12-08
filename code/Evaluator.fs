module Evaluator
open Parser
open System.IO
open Sentiments
open System.Collections

// TODO: just doesnt translate or warn

// type EmphAndPOS = {Emph: string; POS: string}

// lets put everything in twice --> once with real rhyme match the other with [] rhyme match to match w things that dont need to rhyme
type EmphPOSAndRhyme = {emph: string; pos: string; rhyme: string}
// hashmaps are word -> EmphPOSAndRhyme

let MATCH_POS = true
let SEE_TRANSLATION = false
// type Entry = { word: string; emphAndPOS: EmphAndPOS; rhyme: string List}
// type Entry = { word: string; emphPOSAndRhyme: EmphPOSAndRhyme}

// type Entry = { word: string; emphAndPOS: EmphAndPOS; rhyme: string List}
// type features = { emphAndPOS: EmphAndPOS; rhyme: string}

let reuse = new Hashtable()

let update_hashmap (map:Hashtable) key toadd =         
    if map.ContainsKey(key) then
        let old: string list = map[key] |> unbox
        map.Remove(key)
        map.Add(key, box (toadd::old))

    else
        map.Add(key, box [toadd])
// run checker for if words are in dictionary before running evaluator
let readDict (preferredArr: string list) =
    let wordToFeature = new Hashtable()
    let featToWord = new Hashtable()
    let commonFeatToWord = new Hashtable()
    let preferredFeatToWord = new Hashtable()


    let dict = File.ReadAllText "cmu_pos_dict.txt"
    let common = File.ReadAllText "new_common.txt"


    let dictArr = dict.Split('\n')
    let commonArr = common.Split('\n')


    let dummy = dictArr |> Array.map (fun i -> 
        let spl: string list = i.Split(' ') |> Array.toList
        let len = (List.length spl)
        let myRhyme:string list = (if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]])
        let myStringRhyme = String.concat "" myRhyme 
        let myPOS = spl[1]
        let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
        let myEmphPOSandRhyme = {emph = myEmph; pos = myPOS; rhyme = myStringRhyme}
        let myEmphPOSandNoRhyme = {emph = myEmph; pos = myPOS; rhyme = ""}

        // TODO: make helper
        wordToFeature.Add(spl[0], box myEmphPOSandRhyme)

        update_hashmap featToWord myEmphPOSandRhyme spl[0]

        update_hashmap featToWord myEmphPOSandNoRhyme spl[0]

        )

    let dummy = commonArr |> Array.map (fun i ->
        if wordToFeature.ContainsKey((i.ToUpper())) then
            let feat: EmphPOSAndRhyme = wordToFeature[(i.ToUpper())] |> unbox
            let noRhymeFeat:EmphPOSAndRhyme = {emph = feat.emph; pos = feat.pos; rhyme = ""}
            // let myEmph = feat.emphAndPOS
            update_hashmap commonFeatToWord feat i
            update_hashmap commonFeatToWord noRhymeFeat i
        else 
            printfn "Warning: %A does not exist in the dictionary" i

        )

    let dummy = preferredArr |> List.map (fun i ->
        if wordToFeature.ContainsKey((i.ToUpper())) then
            let feat: EmphPOSAndRhyme = wordToFeature[(i.ToUpper())] |> unbox
            let noRhymeFeat:EmphPOSAndRhyme = {emph = feat.emph; pos = feat.pos; rhyme = ""}

            update_hashmap commonFeatToWord feat i
            update_hashmap commonFeatToWord noRhymeFeat i
        else 
            printfn "Warning: %A does not exist in the dictionary" i
        )
    
        
    (wordToFeature, featToWord, commonFeatToWord, preferredFeatToWord)

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
                        let myFeat: EmphPOSAndRhyme = wtf[(x.word.ToUpper())] |> unbox
                        
                        let correctFeat = if (x.rhyme) then myFeat else {emph = myFeat.emph; pos = myFeat.pos; rhyme = ""}
                        // printfn "%A" correctFeat
                        // let (myEmphAndPOS: EmphAndPOS) = myWord.emphAndPOS
                        // TODO: dont use hash if the only entry is it
                        let translator = (
                            if (petw.ContainsKey(correctFeat) && ((unbox petw[correctFeat]): string list).Length > 1) then
                                petw
                            else if (cetw.ContainsKey(correctFeat) && ((unbox cetw[correctFeat]): string list).Length > 1) then
                                cetw
                            else
                                etw
                            )
                        let wordList: string list = (translator[correctFeat]) |> unbox
                        let len = wordList.Length
                        let ind = rnd.Next() % (len)
                        let newWord = wordList[ind] 
                        let fixedWord = 
                            if (newWord = x.word) then
                                if (len > 1) then
                                    wordList[(ind + 1) % len]
                                else 
                                    printfn "Warning: no match was found for %A" x.word
                                    newWord
                            else 
                                newWord


                        let updatedWordList = wordList |> List.filter(fun x -> x <> fixedWord)
                        translator.Remove(correctFeat)
                        if updatedWordList.Length <> 0 then
                            translator.Add(correctFeat, updatedWordList)
                        reuse.Add(x.word, fixedWord)
                        if SEE_TRANSLATION then printfn "%A, %A, %A" x.word fixedWord correctFeat
                        
                        fixedWord::(helpConvert xs)
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
    | x::xs ->( x |> String.concat " ").ToLower() + "\n" + (prettyprint xs)