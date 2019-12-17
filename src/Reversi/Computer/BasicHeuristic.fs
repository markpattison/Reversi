module FableReversi.Reversi.Computer.Heuristics.Basic

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let corners board =
    [
        board.Squares.[0]
        board.Squares.[7]
        board.Squares.[56]
        board.Squares.[63]
    ]

let heuristicOngoing (ongoing: OngoingGame) =
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
                |> Array.map (fun pm -> minimaxCalc (depth + 1) pm.Result)
            let bestScore = if board.NextToMove = Black then Array.max scores else Array.min scores
            log.Log depth (sprintf "Depth %i, next to move: %O, score: %.1f" depth board.NextToMove bestScore)
            bestScore

    (minimaxCalc 0 board, heuristicEvaluations)

let createWithLog log depth =
    let random = new System.Random()
    let mutable moveIndex = 0
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        ChooseMove = fun ongoingGame ->

            let movesWithScoresAndEvaluations =
                ongoingGame.PossibleMoves
                |> Array.map (fun pm -> (pm, pm.Result |> minimax log depth))

            let totalHeuristicEvaluations = movesWithScoresAndEvaluations |> Array.sumBy (fun (_, (_, evaluations)) -> evaluations)

            let movesWithScores = movesWithScoresAndEvaluations |> Array.map (fun (move, (score, _)) -> move, score)
            let scores = movesWithScores |> Array.map snd
            let bestScore = match ongoingGame.Board.NextToMove with | Black -> Array.max scores | White -> Array.min scores

            let bestMoves =
                movesWithScores
                |> Array.filter (fun (_, score) -> score = bestScore)
                |> Array.map fst

            let choice = random.Next(0, bestMoves.Length)

            moveIndex <- moveIndex + 1

            log.Log -1 (sprintf "Move %i: %i heuristic evaluations, score: %.1fs" moveIndex totalHeuristicEvaluations bestScore)

            bestMoves.[choice]
    }

let create depth = createWithLog (Logger.Create()) depth
