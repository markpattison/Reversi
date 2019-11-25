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

let formatScores scores = sprintf "{%s}" (System.String.Join(",", scores |> List.map (fun s -> sprintf "%.1f" s)))

let minimax log depth board =
    let mutable heuristicEvaluations = 0

    let rec minimaxCalc depth board =
        let gameInfo = Board.toGameInfo board

        match gameInfo.State with
        | OngoingSkipMove _ -> minimaxCalc depth (Actions.skipMove gameInfo)
        | Finished f ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = heuristicFinished f
            kprintf log "Depth %i, score: %.1f (game over)" depth score
            score
        | Ongoing ongoing when depth <= 0 ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = heuristicOngoing ongoing
            kprintf log "Depth %i, score: %.1f" depth score
            score
        | Ongoing ongoing ->
            let scores =
                ongoing.PossibleMoves
                |> List.map (fun pm -> minimaxCalc (depth - 1) pm.Result)
            let bestScore = if board.NextToMove = Black then List.max scores else List.min scores
            kprintf log "Depth %i, next to move: %O, best score: %.1f, options: %s" depth board.NextToMove bestScore (formatScores scores)
            bestScore

    (minimaxCalc depth board, heuristicEvaluations)

let createWithLog log depth =
    let random = new System.Random()
    let mutable moveIndex = 0
    let stopwatch = System.Diagnostics.Stopwatch()
    {
        ChooseMove = fun ongoingGame ->
            let elapsedStart = stopwatch.ElapsedMilliseconds
            stopwatch.Start()

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

            stopwatch.Stop()
            kprintf log "Move %i: %i heuristic evaluations, score: %.1fs, options: %s, time: %.2fs " moveIndex totalHeuristicEvaluations bestScore (formatScores scores) (float (stopwatch.ElapsedMilliseconds - elapsedStart) / 1000.0)

            bestMoves.Item choice
    }

let dummyLogger = fun _ -> ()

let create depth = createWithLog dummyLogger depth
