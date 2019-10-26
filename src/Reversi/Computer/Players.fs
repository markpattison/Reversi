module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | BasicHeuristic

let all =
    [
        Random, "Random"
        Greedy, "Greedy"
        FewestReplies, "FewestReplies"
        BasicHeuristic, "BasicHeuristic"
    ]

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | BasicHeuristic -> Heuristics.Basic.create()
