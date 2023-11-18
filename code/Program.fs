open Parser
open Evaluator
open Combinator
open System.IO

let rec repl() : unit =
    printf "Enter an expression: "
    let input = System.Console.ReadLine()
    if input = "quit" then
        printfn "exiting"
        exit 0
    else
        let astOp = parse input
        match astOp with
        | Some ast -> printfn "%A" (ast)
        | None -> ()
    repl()

[<EntryPoint>]
let main args =
    let file = args.[0]
    let input = File.ReadAllText file

    let astOp = parse input
    match astOp with
    | Some ast -> printfn "%A" (ast)
    | None -> ()
    0