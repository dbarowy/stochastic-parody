module Evaluator
open Parser
open System.IO

type Entry = { word: string; emph: string; rhyme: string List}

// let readDict = 
//     let dict = File.ReadAllText "cmu_dict"
//     let dictList = dict.Split('\n')
//     // printfn "%A" (dictList[53690].Split(' '))
//     let rec helper (dl: string list) = 
//         match dl with 
//         | [] -> []
//         | x::xs ->
//             let spl: string list = x.Split(' ') |> Array.toList
//             // let rec forLoop l =
//             let len = (List.length spl)
//             let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
//             let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
//             {word = spl[0]; emph = myEmph; rhyme = myRhyme} :: (helper xs)
    
//     let x = helper (dictList[0..10000] |> Array.toList)
//     // printfn "%A" x[10000]
//     x


let readDict =
    let dict = File.ReadAllText "cmu_dict"
    let dictArr = dict.Split('\n')
    let formatArr: Entry array = Array.create (Array.length dictArr) {word = ""; emph = ""; rhyme = [""]}

    for i in 0.. (Array.length dictArr) - 1 do
        let spl: string list = dictArr[i].Split(' ') |> Array.toList
        let len = (List.length spl)
        let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
        let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
        formatArr[i] <- {word = spl[0]; emph = myEmph; rhyme = myRhyme}
    formatArr
// let readDict = 
//     let dict = File.ReadAllText "cmu_dict"
//     let dictList = dict.Split('\n')
//     // printfn "%A" (dictList[53690].Split(' '))
//     let mutable arrOfEntries = [||]
//     let rec helper (dl) = 
//         match dl with 
//         | x when Array.isEmpty dl -> ()
//         | x ->
//             let spl: string list = x[0].Split(' ') |> Array.toList
//             // let rec forLoop l =
//             let len = (List.length spl)
//             let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
//             let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
//             arrOfEntries = Array.append arrOfEntries {word = spl[0]; emph = myEmph; rhyme = myRhyme}
//             helper (xs)

//     let x = helper (dictList)
//     // printfn "%A" x[10000]
//     x

let evalSentiment sent = 
    if sent = "happy" then ["adore"; "apple"; "atop"] else []


let evalKeywords kws = 
    kws

let rec searchWord n (word: string) (cmuDict: Entry array) = 
    // match cmuDict with
    // | x::xs when x.word = word -> x
    // | [] -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    // | x::xs -> searchWord word xs
    match n with
    | 0 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    | i -> 
        if (cmuDict[i].word = word.ToUpper()) then cmuDict[i] else searchWord (n - 1)  word cmuDict 


let rec searchEmph emph n (word: string) (cmuDict: Entry array) = 
    match n with
    | 0 -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    | i -> if cmuDict[i].emph = emph && cmuDict[i].word <> word.ToUpper() then cmuDict[i] else searchEmph emph (n - 1) word cmuDict 
    // | x::xs when (x.emph = emph) && (x.word <> word) -> x
    // | [] -> {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    // | x::xs -> searchEmph emph word xs
    // for i in cmuDict do
    //     if i.emph = emph && i.word <> word then i

    // {word = "FAIL"; emph = "FAIL"; rhyme = ["FAIL"]}
    // cmuDict |> Array.fold (fun acc x ->
    //     match x with 
    //     | x when (x.emph = emph) && (x.word <> word) -> acc 
    //     )
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
                // helper (xs, (words @ (evalSentiment y)))
                helper xs

                // helper xs
            | Keywords y -> 
                // helper (xs, (words @ ((evalKeywords y))))
                helper xs

                // wordList := (evalKeywords x) @ wordList
                // helper xs
            | Line x ->
                (convert x cmuDict) :: helper xs
                // "TODO" :: helper xs

    // printfn "%A" (wordList)
    (helper ps[2..])
    // let x = readDict
    // convert ["apple"; "boston"] [{word = "apple"; emph = "01"; rhyme = ["FAIL"]}; {word = "boston"; emph = "01"; rhyme = ["FAIL"]}]

        

                
            