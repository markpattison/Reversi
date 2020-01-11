module FableReversi.TestComputer.TestMCTS

open Expecto
open FableReversi.Reversi.Computer
open FableReversi.TestComputer.Series

let allTests =
    testList "MCTS player" [
        testCase "random vs. MCTS" (fun _ ->
            let series =
                playSeries
                    Players.Random
                    (Players.MCTS 25)
                    1

            ()
        )

        testCase "BasicHeuristic depth 2 vs. MCTS 15" (fun _ ->
            let series =
                playSeries
                    (Players.Minimax (Heuristics.Basic, 2))
                    (Players.MCTS 15)
                    5

            ()
        )

        testCase "BasicHeuristic depth 3 vs. MCTS 200" (fun _ ->
            let series =
                playSeries
                    (Players.Minimax (Heuristics.Basic, 3))
                    (Players.MCTS 200)
                    5

            ()
        )
    ]