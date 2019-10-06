namespace FableReversi.Reversi.Computer

open FableReversi.Reversi

type ComputerPlayer =
    {
        ChooseMove: Board -> PossibleMove
    }
