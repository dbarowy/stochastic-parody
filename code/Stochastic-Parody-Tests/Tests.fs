namespace Stochastic_Parody_Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Helper
open AST
open Evaluator


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.correctlyParsesLine () =
        let input = "Just a $small !town girl\n"
        let expected = [    
                            Sentiment "";
                            Keywords [];
                            Line [{word = "Just"; translate = false; rhyme = false};
                            {word = "a"; translate = false; rhyme = false};
                            {word = "small"; translate = true; rhyme = true};
                            {word = "town"; translate = true; rhyme = false};
                            {word = "girl"; translate = false; rhyme = false};]
                        ]
        let result = parse input
        match result with
        | Some output -> Assert.AreEqual(expected, output)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.canParseAnyLineWithoutSymbols () =
        let rnd = System.Random()
        let reps = [0..10000]
        let allSucceeded: bool = 
                reps |> List.fold (fun acc x -> 
                    let l = generateRandomLine rnd
                    let result = parse l
                    match result with
                    | Some output -> (acc && true)
                    | None -> (acc && false)
                ) true

        match allSucceeded with
        | true -> Assert.IsTrue(true)
        | false -> Assert.IsTrue(false)


    [<TestMethod>]
    member this.parserDoesntThrowException () =
        let rnd = System.Random()
        let reps = [0..10000]
        try 
            let allSucceeded = 
                    reps |> List.map (fun x -> 
                        let l = generateRandomLineWithSymbols rnd
                        let result = parse l
                        l)
            ()
        with
        | ex -> Assert.IsTrue(false)

        Assert.IsTrue(true)

    [<TestMethod>]
    member this.correctlyEvaluatesExpressionUsingPreferredList () =

        let input = [    
                            Sentiment "";
                            Keywords ["pearl"];
                            Line [{word = "Just"; translate = false; rhyme = false};
                            {word = "a"; translate = false; rhyme = false};
                            {word = "small"; translate = false; rhyme = false};
                            {word = "town"; translate = false; rhyme = false};
                            {word = "girl"; translate = true; rhyme = true};]
                        ]
        let expected = "just a small town pearl\n"

        let result = prettyprint (evalProg input false)

        Assert.AreEqual(expected, result)



    [<TestMethod>]
    member this.EvaluatorDoesntThrowException () =
        let rnd = System.Random()
        let reps = [0..10000]
        let randomInput: string = 
                reps |> List.fold (fun acc x -> 
                    let l = generateRandomLine rnd
                    acc + l
                ) ""
        
        let result = parse randomInput
        try 
            let eval = match result with
                        | Some output -> (evalProg output false)
                        | None -> [[]]
            if eval = [[]] then Assert.IsTrue(false)
            printfn "%A" (prettyprint eval)
        with 
        | ex -> Assert.IsTrue(false)
        
        Assert.IsTrue(true)
