module FableReversi.State

open Elmish

open FableReversi.Reversi
open Lobby.Types
open Game.Types
open Types

let createPlayer playerChoice =
    match playerChoice with
    | HumanChoice -> "Human", Human
    | ComputerChoice c -> sprintf "Computer %A" c, Computer (Computer.Players.create c)

let init () =
    let initialOuterModel = { OuterState = Lobby (Lobby.State.init()) }
    initialOuterModel, Cmd.none

let newGame blackPlayer whitePlayer =
    let startingBoard = Board.startingBoard
    let gameInfo = Board.toGameInfo startingBoard
    let black = createPlayer blackPlayer
    let white = createPlayer whitePlayer
    let uniqueId = ref 0

    let model =
        { GameInfo = gameInfo
          BoardView = Game.State.toBoardView gameInfo
          PlayerBlackChoice = blackPlayer
          PlayerWhiteChoice = whitePlayer
          PlayerBlack = black
          PlayerWhite = white
          BlackDescription = (snd black).Describe() |> Array.map (Game.State.toDescriptionView uniqueId)
          WhiteDescription = (snd white).Describe() |> Array.map (Game.State.toDescriptionView uniqueId)
          UniqueId = uniqueId }

    { OuterState = Playing model }, Cmd.ofMsg (GameMsg RequestComputerMoveIfNeeded)

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
