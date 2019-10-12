module FableReversi.Reversi.Computer.Greedy

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = new System.Random()
    {
        ChooseMove = fun ongoingGame ->
            let movesWithMostFlips =
                ongoingGame.PossibleMoves
                |> List.groupBy (fun pm -> pm.Flips.Length)
                |> List.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithMostFlips.Length)
            movesWithMostFlips.Item choice
    }
