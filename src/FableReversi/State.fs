module FableReversi.State

open Elmish

open Lobby.Types
open Game.Types
open Types

let init () =
    let initialOuterModel = { OuterState = Lobby (Lobby.State.init()) }
    initialOuterModel, Cmd.none

let newGame blackPlayer whitePlayer =
    let gameModel = Game.State.newGame blackPlayer whitePlayer
    { OuterState = Playing gameModel }, Cmd.ofMsg (GameMsg RequestComputerMoveIfNeeded)

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg, model.OuterState with
    | LobbyMsg Start, Lobby { PlayerBlackChoice = blackPlayer; PlayerWhiteChoice = whitePlayer } ->
        newGame blackPlayer whitePlayer

    | GameMsg Restart, Playing gameModel ->
        // swap players
        let blackPlayer = gameModel.PlayerWhiteChoice
        let whitePlayer = gameModel.PlayerBlackChoice
        
        newGame blackPlayer whitePlayer

    | GameMsg ChangePlayers, Playing _ ->
        init()

    | LobbyMsg lobbyMsg, Lobby lobbyOptions ->
        let updatedLobbyOptions, cmd = Lobby.State.update lobbyMsg lobbyOptions
        { model with OuterState = Lobby updatedLobbyOptions }, Cmd.map LobbyMsg cmd

    | GameMsg gameMsg, Playing gameModel ->
        let updatedGameModel, cmd = Game.State.update gameMsg gameModel
        { model with OuterState = Playing updatedGameModel }, Cmd.map GameMsg cmd

    | LobbyMsg _, Playing _ -> model, Cmd.none
    | GameMsg _, Lobby _ -> model, Cmd.none
