module FableReversi.Reversi.Computer.Heuristics.Basic

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let heuristicOngoing (ongoing: OngoingGame) =
    float (ongoing.Board.NumBlack - ongoing.Board.NumWhite) // TODO: improve

let heuristicFinished (finished: FinishedGame) =
    match finished.Result with
    | Tie -> 0.0
    | Win Black -> 1000.0 + float (finished.Board.NumBlack - finished.Board.NumWhite)
    | Win White -> -1000.0 + float (finished.Board.NumBlack - finished.Board.NumWhite)

let rec minimax depth board =
    let gameInfo = Board.toGameInfo board

    match gameInfo.State with
    | OngoingSkipMove _ -> minimax depth (Actions.skipMove gameInfo)
    | Finished f -> heuristicFinished f
    | Ongoing ongoing when depth <= 0 -> heuristicOngoing ongoing
    | Ongoing ongoing ->
        let switch = if board.NextToMove = Black then 1.0 else -1.0
        let value =
            ongoing.PossibleMoves
            |> List.map (fun pm -> switch * minimax (depth - 1) pm.Result)
            |> List.max
        value * switch

let create depth =
    let random = new System.Random()
    {
        ChooseMove = fun ongoingGame ->
            let movesWithScores =
                ongoingGame.PossibleMoves
                |> List.groupBy (fun pm -> pm.Result |> minimax depth)
            
            let bestMoves =
                match ongoingGame.Board.NextToMove with
                | Black -> movesWithScores |> List.maxBy fst |> snd
                | White -> movesWithScores |> List.minBy fst |> snd
            
            let choice = random.Next(0, bestMoves.Length)
            bestMoves.Item choice
    }
