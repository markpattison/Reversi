module FableReversi.Game.Types

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open FableReversi.Lobby.Types

type Square =
    | Empty
    | Piece of Colour

type SquareView =
    | PossibleMove
    | PossibleMoveHover
    | WouldFlip
    | Plain

type BoardView =
    { SquareViews: ((int * int) * Square * SquareView) list list }

type Player =
    | Human
    | Computer of ComputerPlayer
    with
        member this.Describe() =
            match this with
            | Human -> [||]
            | Computer player -> player.Describe()

type DescriptionView =
    {
      Id: int
      TextView : string
      SubDescriptionsView : (bool  * DescriptionView []) option
    }

type GameModel =
    { GameInfo: GameInfo
      BoardView: BoardView
      PlayerBlackChoice: PlayerChoice
      PlayerWhiteChoice: PlayerChoice
      PlayerBlack: string * Player
      PlayerWhite: string * Player
      BlackDescription : DescriptionView []
      WhiteDescription : DescriptionView []
      UniqueId: int ref }

    member this.CurrentPlayer =
        match this.GameInfo.Board.NextToMove with
        | Black -> this.PlayerBlack |> snd
        | White -> this.PlayerWhite |> snd

type GameMsg =
    | Hover of int * int
    | Click of int * int
    | GameAction of GameAction
    | Expand of int
    | RequestComputerMoveIfNeeded
    | Restart
    | ChangePlayers
