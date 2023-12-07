module Evaluator
open Parser
open System.IO

open System.Collections

type wordAndPOS =
| Noun of string
| Adj of string
| Verb of string
| Adv of string
| Other of string

// type wordAndPOS = {word : string; partOfSpeech: PartOfSpeech}
type Entry = { word: string; emph: string; rhyme: string List}
type features = { emph: string; rhyme: string List}

let reuse = new Hashtable()



// run checker for if words are in dictionary before running evaluator
let readDict (preferredArr: string list) =
    let wordToFeature = new Hashtable()
    let emphToWord = new Hashtable()
    let commonEmphToWord = new Hashtable()
    let preferredEmphToWord = new Hashtable()


    let dict = File.ReadAllText "cmu_dict"
    let common = File.ReadAllText "common.txt"

    let noun_dict = File.ReadAllText "dict_noun.txt"
    let adj_dict = File.ReadAllText "dict_adj.txt"
    let verb_dict = File.ReadAllText "dict_verb.txt"
    let adv_dict = File.ReadAllText "dict_adv.txt"



    let dictArr = dict.Split('\n')
    let commonArr = common.Split('\n')
    
    let nounKey = noun_dict.Split('\n')
    let adjKey = adj_dict.Split('\n')
    let verbKey = verb_dict.Split('\n')
    let advKey = adv_dict.Split('\n')


    let addPOStoWord (word:string): wordAndPOS = 
    // wordList |> List.filter(fun x -> x <> newWord)
        if (nounKey |> Array.filter(fun x -> x = word) |> Array.length) <> 0 then 
            Noun word
        else if (adjKey |> Array.filter(fun x -> x = word) |> Array.length) <> 0 then 
            Adj word
        else if (verbKey |> Array.filter(fun x -> x = word) |> Array.length) <> 0 then 
            Verb word
        else if (advKey |> Array.filter(fun x -> x = word) |> Array.length) <> 0 then 
            Adv word
        else 
            Other word

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

    let dummy = preferredArr |> List.map (fun i ->
        let feat: features = wordToFeature[(i.ToUpper())] |> unbox
        let myEmph = feat.emph
        if preferredEmphToWord.ContainsKey(myEmph) then
            //  (emphToWord[myEmph] = spl[0]::(emphToWord[myEmph]))
           let old: string list = preferredEmphToWord[myEmph] |> unbox
           preferredEmphToWord.Remove(myEmph)
           preferredEmphToWord.Add(myEmph, box (i::old))

        else
            preferredEmphToWord.Add(myEmph, box [i])
        
    
        )
    
        
    (wordToFeature, emphToWord, commonEmphToWord, preferredEmphToWord)

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
let convert (lines: TranslationUnit list) (wtf: Hashtable) (etw: Hashtable) (cetw: Hashtable) (petw: Hashtable)=
    // let len = ((Array.length cmuDict) - 1)

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
                        let myWord: features = wtf[(x.word.ToUpper())] |> unbox // used to be len
                        let (myEmph: string) = myWord.emph
                        // printfn "%A" myWord
                        let translator = if petw.ContainsKey(myEmph) then petw else if cetw.ContainsKey(myEmph) then cetw else etw
                        let wordList: string list = (translator[myEmph]) |> unbox
                        let len = wordList.Length
                        // printfn "%A" (etw.ContainsKey(myEmph))
                        // printfn "%A" (wordList[0])
                        let newWord = wordList[rnd.Next() % (len)] 

                        let updatedWordList = wordList |> List.filter(fun x -> x <> newWord)
                        translator.Remove(myEmph)
                        if updatedWordList.Length <> 0 then
                            translator.Add(myEmph, updatedWordList)
                        reuse.Add(x.word, newWord)
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

    let wtf, etw, cetw, petw = readDict wordList
    
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


        

let rec prettyprint (p: string list list): string=
    match p with
    | [] -> ""
    | x::xs ->( x |> String.concat " ") + "\n" + (prettyprint xs)