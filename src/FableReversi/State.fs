module FableReversi.State

open Elmish

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open Types

let getPossibleMoves gameInfo =
    match gameInfo.State with
    | Ongoing { PossibleMoves = moves } -> moves
    | _ -> []

let getPossibleMoveLocations gameInfo =
    getPossibleMoves gameInfo
    |> List.map (fun pm -> pm.MoveLocation)

let toBoardView gameInfo =
    let possibleMoveLocations = getPossibleMoveLocations gameInfo
    
    { SquareViews =
        List.init 8 (fun i ->
            let y = 7 - i
            List.init 8 (fun x ->
                let location = Location (x, y)
                let view =
                    if List.contains location possibleMoveLocations then
                        PossibleMove
                    else
                        Plain
                (location, Board.squareAt gameInfo.Board location, view))) }

let toBoardViewPossibleMoveHover gameInfo possibleMove =
    let possibleMoveLocations = getPossibleMoveLocations gameInfo

    { SquareViews =
        List.init 8 (fun i ->
            let y = 7 - i
            List.init 8 (fun x ->
                let location = Location (x, y)
                let view =
                    if location = possibleMove.MoveLocation then
                        PossibleMoveHover
                    elif List.contains location possibleMove.Flips then
                        WouldFlip
                    elif List.contains location possibleMoveLocations then
                        PossibleMove
                    else
                        Plain
                (location, Board.squareAt gameInfo.Board location, view))) }

let createPlayer playerChoice =
    match playerChoice with
    | HumanChoice -> "Human", Human
    | ComputerChoice c -> sprintf "Computer %A" c, Computer (Computer.Players.Create c)

let init () =
    let initialOuterModel =
        { OuterState =
            Lobby
                { PlayerBlackChoice = HumanChoice
                  PlayerWhiteChoice = HumanChoice } }
    initialOuterModel, Cmd.none

let updateBoard model board =
    let gameInfo = Board.toGameInfo board
    { model with GameInfo = gameInfo; BoardView = toBoardView gameInfo }

let requestComputerMove (player: ComputerPlayer, board) =
    async {
        do! Async.Sleep 100
        return player.ChooseMove board
    }

let updateLobby (msg: LobbyMsg) (options: LobbyOptions) : LobbyOptions * Cmd<LobbyMsg> =
    match msg with
    | ChangeBlackPlayer p -> { options with PlayerBlackChoice = p }, Cmd.none
    | ChangeWhitePlayer p -> { options with PlayerWhiteChoice = p }, Cmd.none
    | Start -> options, Cmd.none // handled at Model level

let updateGame (msg : GameMsg) (model : GameModel) : GameModel * Cmd<GameMsg> =
    let possibleMoves = getPossibleMoves model.GameInfo

    match msg with

    | Hover location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) possibleMoves with
        | Some possibleMove ->
            let boardView = toBoardViewPossibleMoveHover model.GameInfo possibleMove
            { model with BoardView = boardView }, Cmd.none
        
        | None -> { model with BoardView = toBoardView model.GameInfo }, Cmd.none
    
    | Click location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) possibleMoves with
        | Some possibleMove -> model, Cmd.ofMsg (GameAction (PlayMove possibleMove))
        | None -> model, Cmd.none

    | GameAction action ->
        let newModel =
            match action, model.GameInfo.State with
            | PlayMove possibleMove, Ongoing _ when List.exists (fun pm -> pm.MoveLocation = possibleMove.MoveLocation) possibleMoves -> updateBoard model possibleMove.Result
            | SkipMove, OngoingSkipMove _ -> updateBoard model (Actions.skipMove model.GameInfo)
            | _ -> model

        newModel, Cmd.ofMsg RequestComputerMoveIfNeeded
    
    | RequestComputerMoveIfNeeded ->
        let computerRequest =
            match model.GameInfo.State, model.CurrentPlayer with
            | _, Human -> Cmd.none
            | Finished _, _ -> Cmd.none
            | OngoingSkipMove _, _ -> Cmd.ofMsg (GameAction SkipMove)
            | Ongoing ongoingGame, Computer player ->
                Cmd.OfAsync.perform requestComputerMove (player, ongoingGame) (fun move -> GameAction (PlayMove move))
        
        model, computerRequest

    | Restart -> model, Cmd.none // handled at Model level
    | ChangePlayers -> model, Cmd.none // handled at Model level

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg, model.OuterState with
    | LobbyMsg Start, Lobby { PlayerBlackChoice = blackPlayer; PlayerWhiteChoice = whitePlayer } -> 
        let startingBoard = Board.startingBoard
        let gameInfo = Board.toGameInfo startingBoard

        let initialModel =
            { GameInfo = gameInfo
              BoardView = toBoardView gameInfo
              PlayerBlack = createPlayer blackPlayer
              PlayerWhite = createPlayer whitePlayer }
        
        { OuterState = Playing initialModel }, Cmd.ofMsg (GameMsg RequestComputerMoveIfNeeded)
    
    | GameMsg Restart, Playing gameModel ->
        let startingBoard = Board.startingBoard
        let gameInfo = Board.toGameInfo startingBoard

        let initialModel =
            { GameInfo = gameInfo
              BoardView = toBoardView gameInfo
              PlayerBlack = gameModel.PlayerWhite
              PlayerWhite = gameModel.PlayerBlack }
        
        { OuterState = Playing initialModel }, Cmd.ofMsg (GameMsg RequestComputerMoveIfNeeded)

    | GameMsg ChangePlayers, Playing _ -> init()

    | LobbyMsg lobbyMsg, Lobby lobbyOptions ->
        let updatedLobbyOptions, cmd = updateLobby lobbyMsg lobbyOptions
        { model with OuterState = Lobby updatedLobbyOptions }, Cmd.map LobbyMsg cmd
    
    | GameMsg gameMsg, Playing gameModel ->
        let updatedGameModel, cmd = updateGame gameMsg gameModel
        { model with OuterState = Playing updatedGameModel }, Cmd.map GameMsg cmd
    
    | LobbyMsg _, Playing _ -> model, Cmd.none
    | GameMsg _, Lobby _ -> model, Cmd.none
