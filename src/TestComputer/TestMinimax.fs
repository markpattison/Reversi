module FableReversi.TestComputer.TestMinimax

open Expecto
open FableReversi.Reversi
open FableReversi.TestComputer.Helpers

let allTests =
    testList "minimax player" [
        testCase "dummy test" (fun _ ->
            let logger = Logger.Create()

            let playerBlack = fun _ -> Computer.Random.create()
            let playerWhite = fun _ -> Computer.Heuristics.Basic.create 2

            let series = playSeries playerBlack playerWhite 5

            seriesSummary series |> logger.Log -1

            logger.Print -1
        )
    ]