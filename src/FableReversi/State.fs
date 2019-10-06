module FableReversi.State

open Elmish

open FableReversi.Reversi
open Types

let toBoardView (board: Board) =
    let possibleMoves = board.PossibleMoves() |> List.map (fun pm -> pm.MoveLocation)
    { SquareViews =
        List.init board.Size (fun x ->
            List.init board.Size (fun y ->
                let location = Location (x, y)
                let view =
                    if List.contains location possibleMoves then
                        PossibleMove
                    else
                        Plain
                (location, board.Square location, view))) }

let toBoardViewPossibleMoveHover (board: Board) possibleMove =
    let possibleMoves = board.PossibleMoves() |> List.map (fun pm -> pm.MoveLocation)
    { SquareViews =
        List.init board.Size (fun x ->
            List.init board.Size (fun y ->
                let location = Location (x, y)
                let view =
                    if location = possibleMove.MoveLocation then
                        PossibleMoveHover
                    elif List.contains location possibleMove.Flips then
                        WouldFlip
                    elif List.contains location possibleMoves then
                        PossibleMove
                    else
                        Plain
                (location, board.Square location, view))) }
    
let init () =
    let startingBoard = Board()

    let initialModel =
        { Board = startingBoard
          BoardView = toBoardView startingBoard
          PossibleMoves = startingBoard.PossibleMoves()
          GameState = startingBoard.GameState() }
    
    initialModel, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with

    | Hover location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) model.PossibleMoves with
        | Some possibleMove ->
            let boardView = toBoardViewPossibleMoveHover model.Board possibleMove
            { model with BoardView = boardView }, Cmd.none
        
        | None -> { model with BoardView = toBoardView model.Board }, Cmd.none
    
    | Click location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) model.PossibleMoves with
        | Some possibleMove -> model, Cmd.ofMsg (PlayMove possibleMove)
        | None -> model, Cmd.none
    
    | PlayMove possibleMove ->
        let board = possibleMove.Result
        { model with Board = board; PossibleMoves = board.PossibleMoves(); BoardView = toBoardView board; GameState = board.GameState() }, Cmd.none

    | SkipMove ->
        if model.GameState = OngoingSkipMove then
            let board = model.Board.SkipMove()
            { model with Board = board; PossibleMoves = board.PossibleMoves(); BoardView = toBoardView board; GameState = board.GameState() }, Cmd.none
        else
            model, Cmd.none
    
    | RestartGame ->
        if model.GameState = Finished then
            init()
        else
            model, Cmd.none        