module FableReversi.Reversi.Computer

open FableReversi.Reversi

type ComputerAction =
    | ComputerSkipMove
    | ComputerPlayMove of PossibleMove

type ComputerPlayer =
    {
        Play: Board -> PossibleMove
    }

let Play player (board: Board) =
    match board.GameState() with
    | Finished -> failwith "Game finished"
    | OngoingSkipMove -> ComputerSkipMove
    | Ongoing -> player.Play board |> ComputerPlayMove
