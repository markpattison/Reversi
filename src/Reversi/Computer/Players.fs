module FableReversi.Reversi.Computer.Players

open FableReversi.Reversi.Computer.Heuristics

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | Minimax of HeuristicChoice * int
    | MCTS of int
    member this.Name =
        match this with
        | Random -> "Random"
        | Greedy -> "Greedy"
        | FewestReplies -> "FewestReplies"
        | Minimax (heuristic, depth) -> sprintf "Minimax, depth %i, %s heuristic" depth heuristic.Name
        | MCTS playouts -> sprintf "MCTS, %i playouts per turn" playouts

let create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | Minimax (heuristic, depth) -> Minimax.create (createHeuristic heuristic) depth
    | MCTS playouts -> MCTS.create playouts
