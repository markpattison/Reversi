module FableReversi.TestComputer.TestMinimax

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers


let allTests =
    testList "minimax player" [
        testCase "dummy test" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = Computer.Random.create()
            let playerWhite = Computer.Heuristics.Basic.createWithLog logger 2

            printfn "Black: Random, White: BasicHeuristic depth 2"

            for _ in 1..10 do
                let result = playGame playerBlack playerWhite Board.startingBoard

                let summary =
                    match result.Result with
                    | Tie -> "Tie!"
                    | Win Black -> sprintf "Black wins %i-%i" result.Board.NumBlack result.Board.NumWhite
                    | Win White -> sprintf "White wins %i-%i" result.Board.NumWhite result.Board.NumBlack

                logger.Log -1 summary

                logger.Read -1 |> List.iter (fun s -> printfn "%s" s)
        )
    ]