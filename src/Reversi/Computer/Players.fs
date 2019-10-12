module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy

let all =
    [
        Random, "Random"
        Greedy, "Greedy"
    ]

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
