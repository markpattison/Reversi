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
    
let init () =
    let startingBoard = Board.startingBoard
    let gameInfo = Board.toGameInfo startingBoard

    let initialModel =
        { GameInfo = gameInfo
          BoardView = toBoardView gameInfo
          PlayerBlack = Human
          PlayerWhite = Computer Computer.Random.player }
    
    initialModel, Cmd.ofMsg RequestComputerMoveIfNeeded

let updateBoard model board =
    let gameInfo = Board.toGameInfo board
    { model with GameInfo = gameInfo; BoardView = toBoardView gameInfo }

let requestComputerMove (player: ComputerPlayer, board) =
    async {
        do! Async.Sleep 1000
        return player.ChooseMove board
    }

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
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

    | RestartGame ->
        match model.GameInfo.State with
        | Finished _ -> init()
        | _ -> model, Cmd.none
