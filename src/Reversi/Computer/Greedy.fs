module FableReversi.Reversi.Computer.Greedy

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = new System.Random()
    {
        Name="Greedy"
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        ChooseMove = fun ongoingGame ->
            let movesWithMostFlips =
                ongoingGame.PossibleMoves
                |> Array.groupBy (fun pm -> pm.Flips.Length)
                |> Array.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithMostFlips.Length)
            movesWithMostFlips.[choice]
    }
