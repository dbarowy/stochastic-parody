open Parser
open Evaluator
open Combinator
open System.IO

[<EntryPoint>]
let main args =

    // try
    let file = args.[0]
    let input = File.ReadAllText file
    let astOp = parse input
    match astOp with
    | Some ast -> printfn "%A" (prettyprint (evalProg ast))
    | None -> ()

    // let d1, d2 = readDict
    // printfn "%A" d1["HELLO"]
    // with 
    // | ex -> printfn "%A" "Usage: dotnet run <file.song>"

    0