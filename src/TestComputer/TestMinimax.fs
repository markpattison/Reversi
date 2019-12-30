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

            logger.Log -1 "Black: Random, White: BasicHeuristic depth 2"

            for _ in 1..5 do
                let result = playGame playerBlack playerWhite Board.startingBoard

                logger.Log -1 (result.ToString())

            logger.Print -1
        )
    ]