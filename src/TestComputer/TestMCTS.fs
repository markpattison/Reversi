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

            logger.Log -1 "Black: Random, White: BasicMCTS"

            let result = playGame playerBlack playerWhite Board.startingBoard

            logger.Log -1 (result.ToString())

            logger.Print -1
        )

        testCase "BasicHeuristic depth 2 vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = fun _ -> Computer.Heuristics.Basic.createWithLog logger 2
            let playerWhite = fun _ -> Computer.Heuristics.BasicMCTS.createWithLog logger

            logger.Log -1 "Black: BasicHeuristic depth 2, White: BasicMCTS"

            let result = playGame playerBlack playerWhite Board.startingBoard

            logger.Log -1 (result.ToString())

            logger.Print -1
        )
    ]