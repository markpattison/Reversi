module FableReversi.Reversi.Computer.Greedy

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = System.Random()
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        Describe = fun () -> [||]
        ChooseMove = fun ongoingGame ->
            let movesWithMostFlips =
                ongoingGame.PossibleMoves
                |> Array.groupBy (fun pm -> Bitboard.count pm.Flips)
                |> Array.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithMostFlips.Length)
            movesWithMostFlips.[choice]
    }
