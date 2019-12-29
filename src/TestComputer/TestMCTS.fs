module FableReversi.TestComputer.TestMCTS

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers

let allTests =
    testList "MCTS player" [
        testCase "random vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = Computer.Random.create()
            let playerWhite = Computer.Heuristics.BasicMCTS.createWithLog logger

            printfn "Black: Random, White: BasicMCTS"

            let result = playGame playerBlack playerWhite Board.startingBoard

            match result.Result with
            | Tie ->  logger.Log -1 "Tie!"
            | Win Black ->
                logger.Log -1 (sprintf "Black wins %i-%i" result.Board.NumBlack result.Board.NumWhite)
                failwithf "lost againts random player"
            | Win White ->
                logger.Log -1 (sprintf "White wins %i-%i" result.Board.NumWhite result.Board.NumBlack)

            logger.Read -1 |> List.iter (fun s -> printfn "%s" s)
        )

        testCase "BasicHeuristic depth 2 vs. MCTS" (fun _ ->
            printfn "Black: BasicHeuristic depth 2, White: BasicMCTS"

            let logger = Logger.Create()
            let playerBlack = Computer.Heuristics.Basic.createWithLog logger 2
            let playerWhite = Computer.Heuristics.BasicMCTS.createWithLog logger
            let result = playGame playerBlack playerWhite Board.startingBoard

            match result.Result with
            | Tie ->
                logger.Log -1 "Tie!"
            | Win Black ->
                logger.Log -1 (sprintf "Black wins %i-%i" result.Board.NumBlack result.Board.NumWhite)
            | Win White ->
                logger.Log -1 (sprintf "White wins %i-%i" result.Board.NumWhite result.Board.NumBlack)

            logger.Read -1 |> List.iter (fun s -> printfn "%s" s)
        )
    ]