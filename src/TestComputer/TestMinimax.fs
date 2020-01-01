module FableReversi.TestComputer.TestMinimax

open Expecto
open FableReversi.Reversi
open FableReversi.Reversi.Computer
open FableReversi.TestComputer.Helpers

let allTests =
    testList "minimax player" [
        testCase "dummy test" (fun _ ->
            let logger = Logger.Create()

            let series =
                playSeries
                    Players.Random
                    (Players.Minimax (Heuristics.Basic, 2))
                    5

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )
    ]