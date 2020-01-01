module FableReversi.Reversi.Computer.Players

open FableReversi.Reversi.Computer.Heuristics

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | Minimax of HeuristicChoice * int
    | MCTS
    member this.Name =
        match this with
        | Random -> "Random"
        | Greedy -> "Greedy"
        | FewestReplies -> "FewestReplies"
        | Minimax (heuristic, depth) -> sprintf "Minimax, %s heuristic, depth %i" heuristic.Name depth
        | MCTS -> "MCTS"

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | Minimax (heuristic, depth) -> Minimax.create (createHeuristic heuristic) depth
    | MCTS -> MCTS.create()
