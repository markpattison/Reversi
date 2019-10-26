module FableReversi.Reversi.Computer.Heuristics.Basic

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let rec heuristic board =
    let gameInfo = Board.toGameInfo board

    match gameInfo.State with
    | OngoingSkipMove _ -> heuristic (Actions.skipMove gameInfo)
    | Finished { Result = Tie } -> 0.0
    | Finished { Result = Win Black } -> 1000.0 + float (gameInfo.NumBlack - gameInfo.NumWhite)
    | Finished { Result = Win White } -> -1000.0 + float (gameInfo.NumBlack - gameInfo.NumWhite)
    | Ongoing ongoing ->
        float (gameInfo.NumBlack - gameInfo.NumWhite)

let create() =
    let random = new System.Random()
    {
        ChooseMove = fun ongoingGame ->
            let switch = if ongoingGame.Board.NextToMove = Black then 1.0 else -1.0

            let movesWithHighestValue =
                ongoingGame.PossibleMoves
                |> List.groupBy (fun pm -> switch * heuristic pm.Result)
                |> List.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithHighestValue.Length)
            movesWithHighestValue.Item choice
    }
