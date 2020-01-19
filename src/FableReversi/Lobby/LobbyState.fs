module FableReversi.Lobby.State

open Elmish

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open Types

let init () =
    { PlayerBlackChoice = HumanChoice
      PlayerWhiteChoice = HumanChoice }

let update (msg: LobbyMsg) (model: LobbyModel) : LobbyModel * Cmd<LobbyMsg> =
    match msg with
    | ChangeBlackPlayer p -> { model with PlayerBlackChoice = p }, Cmd.none
    | ChangeWhitePlayer p -> { model with PlayerWhiteChoice = p }, Cmd.none
    | Start -> model, Cmd.none // handled at Model level
