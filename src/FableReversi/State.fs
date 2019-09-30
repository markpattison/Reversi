module FableReversi.State

open Elmish

open FableReversi.Reversi
open Types

let toBoardView board =
    { SquareViews = board.Squares |> Array.map (fun sq -> (sq, Plain))
      SizeView = board.Size }

let toBoardViewPossibleMove board possibleMove =
    let flipIndices = possibleMove.Flips |> List.map (Board.indexOf board.Size)
    let squareViews =
        board.Squares |> Array.mapi (fun i sq ->
            let view =
                if i = Board.indexOf board.Size possibleMove.MoveLocation then
                    PossibleMove
                elif List.contains i flipIndices then
                    WouldFlip
                else
                    Plain

            (sq, view))

    { SquareViews = squareViews; SizeView = board.Size }

let init () =
    let startingBoard = Board.createStarting()

    let initialModel =
        { Board = startingBoard
          BoardView = toBoardView startingBoard
          PossibleMoves = Board.possibleMoves startingBoard }
    
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
            let possibleMoves = Board.possibleMoves board
            let boardView = toBoardView board
            { model with Board = board; PossibleMoves = possibleMoves; BoardView = boardView }, Cmd.none
        
        | None -> model, Cmd.none
