module FableReversi.Types

open FableReversi.Reversi

type SquareView =
    | PossibleMove
    | WouldFlip
    | Plain

type BoardView =
    { SquareViews: (Square * SquareView) []
      SizeView: int }
    member this.PieceAt(Location (x, y)) = this.SquareViews.[x + y * this.SizeView]

type Model =
    { Board: Board
      PossibleMoves: PossibleMove list
      BoardView: BoardView }

type Msg =
    | Hover of Location
