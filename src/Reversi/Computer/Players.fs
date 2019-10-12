module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy

let all =
    [
        Random, "Computer random"
        Greedy, "Computer greedy"
    ]

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
