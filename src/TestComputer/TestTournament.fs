module FableReversi.TestComputer.TestTournament

open Expecto
open FableReversi.Reversi.Computer
open FableReversi.TestComputer.Tournament

let allTests =
    testList "tournament" [
        testCase "tournament" (fun _ ->
            let players =
                [
                    Players.Random
                    Players.Greedy
                    Players.FewestReplies
                    (Players.Minimax (Heuristics.Basic, 2))
                    (Players.Minimax (Heuristics.Basic, 3))
                    (Players.MCTS 50)
                    (Players.MCTS 100)
                    (Players.MCTS 200)
                ] |> Set.ofList
            
            let tournament =
                playTournament players 1

            ()
        )
    ]