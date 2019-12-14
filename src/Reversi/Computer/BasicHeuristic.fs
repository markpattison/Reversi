module FableReversi.Reversi.Computer.Heuristics.Basic

open Microsoft.FSharp.Core.Printf
open FableReversi.Reversi
open FableReversi.Reversi.Runner

let heuristicOngoing (ongoing: OngoingGame) =
    float (ongoing.Board.NumBlack - ongoing.Board.NumWhite) // TODO: improve

let heuristicFinished (finished: FinishedGame) =
    match finished.Result with
    | Tie -> 0.0
    | Win Black -> 1000.0 + float (finished.Board.NumBlack - finished.Board.NumWhite)
    | Win White -> -1000.0 + float (finished.Board.NumBlack - finished.Board.NumWhite)

let minimax (log: Logger) maxDepth board =
    let mutable heuristicEvaluations = 0

    let rec minimaxCalc depth board =
        let gameInfo = Board.toGameInfo board

        match gameInfo.State with
        | OngoingSkipMove _ -> minimaxCalc depth (Actions.skipMove gameInfo)
        | Finished f ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = heuristicFinished f
            log.Log depth (sprintf "Depth %i, score: %.1f (game over)" depth score)
            score
        | Ongoing ongoing when depth >= maxDepth ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = heuristicOngoing ongoing
            log.Log depth (sprintf "Depth %i, score: %.1f" depth score)
            score
        | Ongoing ongoing ->
            let scores =
                ongoing.PossibleMoves
                |> List.map (fun pm -> minimaxCalc (depth + 1) pm.Result)
            let bestScore = if board.NextToMove = Black then List.max scores else List.min scores
            log.Log depth (sprintf "Depth %i, next to move: %O, score: %.1f" depth board.NextToMove bestScore)
            bestScore

    (minimaxCalc 0 board, heuristicEvaluations)

let createWithLog log depth =
    let random = new System.Random()
    let mutable moveIndex = 0
    {
        ChooseMove = fun ongoingGame ->

            let movesWithScoresAndEvaluations =
                ongoingGame.PossibleMoves
                |> List.map (fun pm -> (pm, pm.Result |> minimax log depth))

            let totalHeuristicEvaluations = movesWithScoresAndEvaluations |> List.sumBy (fun (_, (_, evaluations)) -> evaluations)
            
            let movesWithScores = movesWithScoresAndEvaluations |> List.map (fun (move, (score, _)) -> move, score)
            let scores = movesWithScores |> List.map snd
            let bestScore = match ongoingGame.Board.NextToMove with | Black -> List.max scores | White -> List.min scores

            let bestMoves =
                movesWithScores
                |> List.filter (fun (_, score) -> score = bestScore)
                |> List.map fst

            let choice = random.Next(0, bestMoves.Length)

            moveIndex <- moveIndex + 1

            log.Log -1 (sprintf "Move %i: %i heuristic evaluations, score: %.1fs" moveIndex totalHeuristicEvaluations bestScore)

            bestMoves.Item choice
    }

let create depth = createWithLog (Logger.Create()) depth
