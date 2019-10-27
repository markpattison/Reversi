namespace FableReversi.Reversi.Runner

open FableReversi.Reversi

type GameAction =
    | SkipMove
    | PlayMove of PossibleMove

type Heuristic = Board -> float

type ComputerPlayer =
    {
        ChooseMove: OngoingGame -> PossibleMove
    }

module Actions =
    let skipMove gameInfo =
        match gameInfo.State with
        | OngoingSkipMove b -> { b with NextToMove = b.NextToMove.opposite }
        | _ -> failwith "Cannot skip move"
