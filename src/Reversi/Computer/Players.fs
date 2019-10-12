module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies

let all =
    [
        Random, "Random"
        Greedy, "Greedy"
        FewestReplies, "FewestReplies"
    ]

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
