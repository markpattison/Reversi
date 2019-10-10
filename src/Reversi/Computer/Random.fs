module FableReversi.Reversi.Computer.Random

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let player =
    let random = new System.Random()
    {
        ChooseMove = fun ongoingGame ->
            let choice = random.Next(0, ongoingGame.PossibleMoves.Length)
            ongoingGame.PossibleMoves.Item choice
    }
