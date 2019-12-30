module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | BasicHeuristic of int
    | BasicMCTS

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | BasicHeuristic depth -> Heuristics.Basic.create depth
    | BasicMCTS -> Heuristics.BasicMCTS.create()
