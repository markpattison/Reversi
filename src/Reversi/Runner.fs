namespace FableReversi.Reversi.Runner

open FableReversi.Reversi

type GameAction =
    | SkipMove
    | PlayMove of PossibleMove

type ComputerPlayer =
    {
        ChooseMove: OngoingGame -> PossibleMove
    }

module Actions =
    let skipMove gameInfo =
        match gameInfo.State with
        | OngoingSkipMove _ ->
            { Squares = gameInfo.Board.Squares; NextToMove = gameInfo.Board.NextToMove.opposite }
        | _ -> failwith "Cannot skip move"
