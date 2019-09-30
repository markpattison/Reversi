module FableReversi.Types

open FableReversi.Reversi

type SquareView =
    | PossibleMove
    | WouldFlip
    | Plain

type BoardView =
    { SquareViews: (Location * Square * SquareView) list list }

type Model =
    { Board: Board
      PossibleMoves: PossibleMove list
      BoardView: BoardView }

type Msg =
    | Hover of Location
    | Click of Location
