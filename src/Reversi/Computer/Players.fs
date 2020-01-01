module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | BasicHeuristic of int
    | BasicMCTS
    member this.Name =
        match this with
        | Random -> "Random"
        | Greedy -> "Greedy"
        | FewestReplies -> "FewestReplies"
        | BasicHeuristic depth -> sprintf "BasicHeuristic depth %i" depth
        | BasicMCTS -> "BasicMCTS"

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | BasicHeuristic depth -> Heuristics.Basic.create depth
    | BasicMCTS -> Heuristics.BasicMCTS.create()
