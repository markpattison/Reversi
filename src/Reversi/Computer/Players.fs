module FableReversi.Reversi.Computer.Players

type ComputerPlayerChoice =
    | Random
    | Greedy
    | FewestReplies
    | BasicHeuristicDepth0
    | BasicHeuristicDepth1
    | BasicHeuristicDepth2
    | BasicMCTS

let Create choice =
    match choice with
    | Random -> Random.create()
    | Greedy -> Greedy.create()
    | FewestReplies -> FewestReplies.create()
    | BasicHeuristicDepth0 -> Heuristics.Basic.create 0
    | BasicHeuristicDepth1 -> Heuristics.Basic.create 1
    | BasicHeuristicDepth2 -> Heuristics.Basic.create 2
    | BasicMCTS -> Heuristics.BasicMCTS.create()
