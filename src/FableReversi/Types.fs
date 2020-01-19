module FableReversi.Types

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open FableReversi.Lobby.Types
open FableReversi.Game.Types

type OuterState =
    | Lobby of LobbyModel
    | Playing of GameModel

type Model =
    { OuterState: OuterState }

type Msg =
    | LobbyMsg of LobbyMsg
    | GameMsg of GameMsg
