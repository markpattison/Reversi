module FableReversi.Reversi.Computer.Runner

open FableReversi.Reversi

type ComputerPlayer =
    {
        ChooseMove: Board -> PossibleMove
    }

let Play player (board: Board) =
    match board.GameState() with
    | Finished -> failwith "Game finished"
    | OngoingSkipMove -> SkipMove
    | Ongoing -> player.ChooseMove board |> PlayMove
