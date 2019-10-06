module FableReversi.Types

open FableReversi.Reversi
open FableReversi.Reversi.Computer

type SquareView =
    | PossibleMove
    | PossibleMoveHover
    | WouldFlip
    | Plain

type BoardView =
    { SquareViews: (Location * Square * SquareView) list list }

type Player =
    | Human
    | Computer of ComputerPlayer

type Model =
    { Board: Board
      PossibleMoves: PossibleMove list
      BoardView: BoardView
      GameState: GameState
      PlayerBlack: Player
      PlayerWhite: Player }
    member this.CurrentPlayer =
        match this.Board.NextToMove with
        | Black -> this.PlayerBlack
        | White -> this.PlayerWhite

type Msg =
    | Hover of Location
    | Click of Location
    | GameAction of GameAction
    | RestartGame
