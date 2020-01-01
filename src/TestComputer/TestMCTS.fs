module FableReversi.TestComputer.TestMCTS

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers

let allTests =
    testList "MCTS player" [
        testCase "random vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let series =
                playSeries
                    Computer.Players.Random
                    Computer.Players.BasicMCTS
                    1

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )

        testCase "BasicHeuristic depth 2 vs. MCTS" (fun _ ->
            let logger = Logger.Create()

            let series =
                playSeries
                    (Computer.Players.BasicHeuristic 2)
                    Computer.Players.BasicMCTS
                    2

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )
    ]