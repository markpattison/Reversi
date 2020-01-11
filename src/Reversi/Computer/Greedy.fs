module FableReversi.Reversi.Computer.Greedy

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = new System.Random()
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        ChooseMove = fun ongoingGame ->
            let movesWithMostFlips =
                ongoingGame.PossibleMoves
                |> Array.groupBy (fun pm -> Bitboard.countStones pm.Flips)
                |> Array.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithMostFlips.Length)
            movesWithMostFlips.[choice]
    }
