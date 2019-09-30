module FableReversi.State

open Elmish

open FableReversi.Reversi
open Types

let toBoardView (board: Board) =
    { SquareViews =
        List.init board.Size (fun x ->
            List.init board.Size (fun y ->
                let location = Location (x, y)
                (location, board.Square location, Plain))) }

let toBoardViewPossibleMove (board: Board) possibleMove =
    { SquareViews =
        List.init board.Size (fun x ->
            List.init board.Size (fun y ->
                let location = Location (x, y)
                let view =
                    if location = possibleMove.MoveLocation then
                        PossibleMove
                    elif List.contains location possibleMove.Flips then
                        WouldFlip
                    else
                        Plain
                (location, board.Square location, view))) }
    
let init () =
    let startingBoard = Board()

    let initialModel =
        { Board = startingBoard
          BoardView = toBoardView startingBoard
          PossibleMoves = startingBoard.PossibleMoves() }
    
    initialModel, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with

    | Hover location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) model.PossibleMoves with
        | Some possibleMove ->
            let boardView = toBoardViewPossibleMove model.Board possibleMove
            { model with BoardView = boardView }, Cmd.none
        
        | None -> { model with BoardView = toBoardView model.Board }, Cmd.none
    
    | Click location ->
        match List.tryFind (fun possibleMove -> possibleMove.MoveLocation = location) model.PossibleMoves with
        | Some possibleMove ->
            let board = possibleMove.Result
            let possibleMoves = board.PossibleMoves()
            let boardView = toBoardView board
            { model with Board = board; PossibleMoves = possibleMoves; BoardView = boardView }, Cmd.none
        
        | None -> model, Cmd.none
