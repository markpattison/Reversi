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

let minimax depth board =
    let mutable heuristicEvaluations = 0

    let rec minimaxCalc depth board =
        let gameInfo = Board.toGameInfo board

        match gameInfo.State with
        | OngoingSkipMove _ -> minimaxCalc depth (Actions.skipMove gameInfo)
        | Finished f ->
            heuristicEvaluations <- heuristicEvaluations + 1
            heuristicFinished f
        | Ongoing ongoing when depth <= 0 ->
            heuristicEvaluations <- heuristicEvaluations + 1
            heuristicOngoing ongoing
        | Ongoing ongoing ->
            let switch = if board.NextToMove = Black then 1.0 else -1.0
            let value =
                ongoing.PossibleMoves
                |> List.map (fun pm -> switch * minimaxCalc (depth - 1) pm.Result)
                |> List.max
            value * switch

    (minimaxCalc depth board, heuristicEvaluations)

let createWithLog logger depth =
    let log = kprintf logger
    let random = new System.Random()
    let mutable moveIndex = 0
    let stopwatch = System.Diagnostics.Stopwatch()
    {
        ChooseMove = fun ongoingGame ->
            let elapsedStart = stopwatch.ElapsedMilliseconds
            stopwatch.Start()

            let movesWithScoresAndEvaluations =
                ongoingGame.PossibleMoves
                |> List.map (fun pm -> (pm, pm.Result |> minimax depth))

            let totalHeuristicEvaluations = movesWithScoresAndEvaluations |> List.sumBy (fun (_, (_, evaluations)) -> evaluations)
            
            let movesWithScores = movesWithScoresAndEvaluations |> List.map (fun (move, (score, _)) -> move, score)

            let bestScore =
                match ongoingGame.Board.NextToMove with
                | Black -> movesWithScores |> List.map snd |> List.max
                | White -> movesWithScores |> List.map snd |> List.min

            let bestMoves =
                movesWithScores
                |> List.filter (fun (_, score) -> score = bestScore)
                |> List.map fst

            let choice = random.Next(0, bestMoves.Length)

            moveIndex <- moveIndex + 1

            stopwatch.Stop()
            log "Move %i: %i heuristic evaluations, %.2fs " moveIndex totalHeuristicEvaluations (float (stopwatch.ElapsedMilliseconds - elapsedStart) / 1000.0)

            bestMoves.Item choice
    }

let dummyLogger = fun _ -> ()

let create depth = createWithLog dummyLogger depth
