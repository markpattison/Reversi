module FableReversi.TestComputer.TestMinimax

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers

let allTests =
    testList "minimax player" [
        testCase "dummy test" (fun _ ->
            let logger = Logger.Create()

            let series =
                playSeries
                    Computer.Players.Random
                    (Computer.Players.BasicHeuristic 2)
                    5

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )
    ]