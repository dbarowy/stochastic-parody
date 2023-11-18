module Evaluator
open System.IO
open Combinator

type Entry = { word: string; emph: string; rhyme: string List}

let readDict = 
    let dict = File.ReadAllText "cmu_dict"
    let dictList = dict.Split('\n')
    // printfn "%A" (dictList[53690].Split(' '))
    let rec helper (dl: string list) = 
        match dl with 
        | [] -> []
        | x::xs ->
            let spl: string list = x.Split(' ') |> Array.toList
            // let rec forLoop l =
            let len = (List.length spl)
            let myRhyme = if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]]
            let myEmph = spl[2..] |> List.map (fun s -> if s.Contains("2") then "2" else (if s.Contains("1") then "1" else (if s.Contains("0") then "0" else ""))) |> (String.concat "")
            {word = spl[0]; emph = myEmph; rhyme = myRhyme} :: (helper xs)
    
    let x = helper (dictList[0..10000] |> Array.toList)
    // printfn "%A" x[0..5]
    0