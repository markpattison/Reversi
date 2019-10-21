module FableReversi.Types

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open FableReversi.Reversi.Computer.Players

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

type PlayerChoice =
    | HumanChoice
    | ComputerChoice of ComputerPlayerChoice

type GameModel =
    { GameInfo: GameInfo
      BoardView: BoardView
      PlayerBlack: string * Player
      PlayerWhite: string * Player }
    member this.CurrentPlayer =
        match this.GameInfo.Board.NextToMove with
        | Black -> this.PlayerBlack |> snd
        | White -> this.PlayerWhite |> snd

type LobbyOptions =
    { PlayerBlackChoice: PlayerChoice
      PlayerWhiteChoice: PlayerChoice }

type OuterState =
    | Lobby of LobbyOptions
    | Playing of GameModel

type Model =
    { OuterState: OuterState }

type LobbyMsg =
    | ChangeBlackPlayer of PlayerChoice
    | ChangeWhitePlayer of PlayerChoice
    | Start

type GameMsg =
    | Hover of Location
    | Click of Location
    | GameAction of GameAction
    | RequestComputerMoveIfNeeded
    | Restart
    | ChangePlayers

type Msg =
    | LobbyMsg of LobbyMsg
    | GameMsg of GameMsg
