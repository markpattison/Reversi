module FableReversi.Types

open FableReversi.Reversi

type SquareView =
    | PossibleMove
    | PossibleMoveHover
    | WouldFlip
    | Plain

type BoardView =
    { SquareViews: (Location * Square * SquareView) list list }

type Model =
    { Board: Board
      PossibleMoves: PossibleMove list
      BoardView: BoardView
      GameState: GameState }

type Msg =
    | Hover of Location
    | Click of Location
    | PlayMove of PossibleMove
    | SkipMove
    | RestartGame
