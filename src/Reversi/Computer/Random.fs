module FableReversi.Reversi.Computer.Random

open FableReversi.Reversi

let player =
    let random = new System.Random()
    {
        Play = fun board ->
            let possibleMoves = board.PossibleMoves()
            let choice = random.Next(0, possibleMoves.Length)
            possibleMoves.Item choice
    }
