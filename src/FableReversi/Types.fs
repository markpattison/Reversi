module FableReversi.Types

open FableReversi.Reversi
open FableReversi.Reversi.Runner

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
    { GameInfo: GameInfo
      BoardView: BoardView
      PlayerBlack: Player
      PlayerWhite: Player }
    member this.CurrentPlayer =
        match this.GameInfo.Board.NextToMove with
        | Black -> this.PlayerBlack
        | White -> this.PlayerWhite

type Msg =
    | Hover of Location
    | Click of Location
    | GameAction of GameAction
    | RequestComputerMoveIfNeeded
    | RestartGame
