module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random

let all =
    [
        Random, "Computer random"
    ]

let Create choice =
    match choice with
    | Random -> Random.create()
