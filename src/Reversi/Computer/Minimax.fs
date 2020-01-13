module FableReversi.Reversi.Computer.Minimax

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let printPos (p:int) =
    let x,y = Bitboard.getXY p
    sprintf "%c%i" (char (65 + x)) (8-y)

let valueFinished (finished: FinishedGame) =
    let numBlack, numWhite = Board.countPieces finished.Board
    match finished.Result with
    | Tie -> 0.0
    | Win Black -> 1000.0 + float (numBlack - numWhite)
    | Win White -> -1000.0 + float (numBlack - numWhite)

let minimax heuristic maxDepth board =
    let mutable heuristicEvaluations = 0

    let rec minimaxCalc depth board =
        let gameInfo = Board.toGameInfo board

        match gameInfo.State with
        | OngoingSkipMove _ -> minimaxCalc depth (Actions.skipMove gameInfo)
        | Finished f ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = valueFinished f
            // printfn depth (sprintf "Depth %i, score: %.1f (game over)" depth score)
            score
        | Ongoing ongoing when depth > maxDepth ->
            heuristicEvaluations <- heuristicEvaluations + 1
            let score = heuristic ongoing
            // printfn depth (sprintf "Depth %i, score: %.1f" depth score)
            score
        | Ongoing ongoing ->
            let scores =
                ongoing.PossibleMoves
                |> Array.map (fun pm -> minimaxCalc (depth + 1) pm.Result)
            let bestScore = if board.NextToMove = Black then Array.max scores else Array.min scores
            // printfn depth (sprintf "Depth %i, next to move: %O, score: %.1f" depth board.NextToMove bestScore)
            bestScore

    (minimaxCalc 1 board, heuristicEvaluations)

let create heuristic depth =
    let random = System.Random()
    let mutable moveIndex = 0
    let mutable lastDecription = [||]

    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        Describe = fun () -> lastDecription
        ChooseMove = fun ongoingGame ->

            let movesWithScoresAndEvaluations =
                ongoingGame.PossibleMoves
                |> Array.map (fun pm -> (pm, pm.Result |> minimax heuristic depth))

            let movesWithScores = movesWithScoresAndEvaluations |> Array.map (fun (move, (score, _)) -> move, score)
            let scores = movesWithScores |> Array.map snd
            let bestScore = match ongoingGame.Board.NextToMove with | Black -> Array.max scores | White -> Array.min scores

            let bestMoves =
                movesWithScores
                |> Array.filter (fun (_, score) -> score = bestScore)
                |> Array.map fst

            let choice = random.Next(0, bestMoves.Length)

            moveIndex <- moveIndex + 1

            lastDecription <-
                [| { Text = sprintf "Evaluation: %.1f" bestScore
                     SubDescriptions =
                        movesWithScores
                        |> Array.sortByDescending snd
                        |> Array.map (fun (move, score) -> { Text = sprintf "%s: %.1f" (printPos move.Pos) score; SubDescriptions = [||] }) } |]

            bestMoves.[choice]
    }
