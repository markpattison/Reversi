module FableReversi.Reversi.Computer.FewestReplies

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let numberPossibleMoves board =
    match (Board.toGameInfo board).State with
    | Ongoing og -> og.PossibleMoves.Length
    | _ -> 0

let create() =
    let random = new System.Random()
    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        ChooseMove = fun ongoingGame ->
            let movesWithFewestReplies =
                ongoingGame.PossibleMoves
                |> Array.groupBy (fun pm -> pm.Result |> numberPossibleMoves)
                |> Array.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithFewestReplies.Length)
            movesWithFewestReplies.[choice]
    }
