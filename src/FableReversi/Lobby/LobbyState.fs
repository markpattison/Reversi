module FableReversi.Lobby.State

open Elmish

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open Types

let init () =
    { PlayerBlackChoice = HumanChoice
      PlayerWhiteChoice = HumanChoice }

let update (msg: LobbyMsg) (options: LobbyModel) : LobbyModel * Cmd<LobbyMsg> =
    match msg with
    | ChangeBlackPlayer p -> { options with PlayerBlackChoice = p }, Cmd.none
    | ChangeWhitePlayer p -> { options with PlayerWhiteChoice = p }, Cmd.none
    | Start -> options, Cmd.none // handled at Model level
