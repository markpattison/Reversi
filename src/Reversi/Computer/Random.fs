module FableReversi.Reversi.Computer.Random

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = System.Random()
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        Describe = fun () -> [||]
        ChooseMove = fun ongoingGame ->
            let choice = random.Next(0, ongoingGame.PossibleMoves.Length)
            ongoingGame.PossibleMoves.[choice]
    }
