module FableReversi.Reversi.Computer.Random

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = new System.Random()
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        ChooseMove = fun ongoingGame ->
            let choice = random.Next(0, ongoingGame.PossibleMoves.Length)
            ongoingGame.PossibleMoves.[choice]
    }
