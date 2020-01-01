module FableReversi.Reversi.Computer.Heuristics

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let corners board =
    [
        board.Squares.[0]
        board.Squares.[7]
        board.Squares.[56]
        board.Squares.[63]
    ]

let basic (ongoing: OngoingGame) =
    let piecesScore = ongoing.Board.NumBlack - ongoing.Board.NumWhite
    let movesScore = ongoing.PossibleMoves.Length
    let cornersScore =
        ongoing.Board
        |> corners
        |> List.sumBy (fun sq ->
            if sq = Piece (ongoing.Board.NextToMove) then 1
            elif sq = Empty then 0
            else -1)

    float (piecesScore + 3 * movesScore + 10 * cornersScore)

type HeuristicChoice =
    | Basic
    member this.Name =
        match this with
        | Basic -> "Basic"

let createHeuristic heuristic : Heuristic =
    match heuristic with
    | Basic -> basic
