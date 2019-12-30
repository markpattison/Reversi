module FableReversi.TestComputer.TestMCTS

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers

let allTests =
    testList "MCTS player" [
        testCase "random vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = fun _ -> Computer.Random.create()
            let playerWhite = fun _ -> Computer.Heuristics.BasicMCTS.createWithLog logger

            let result = playGame playerBlack playerWhite Board.startingBoard

            resultSummary result |> logger.Log -1

            logger.Print -1
        )

        testCase "BasicHeuristic depth 2 vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = fun _ -> Computer.Heuristics.Basic.create 2
            let playerWhite = fun _ -> Computer.Heuristics.BasicMCTS.create()

            let series = playSeries playerBlack playerWhite 2

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )
    ]