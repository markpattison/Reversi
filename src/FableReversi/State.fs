module FableReversi.State

open Elmish

open FableReversi.Reversi
open FableReversi.Reversi.Computer
open Types

let toBoardView (board: Board) =
    let possibleMoves = board.PossibleMoves() |> List.map (fun pm -> pm.MoveLocation)
    { SquareViews =
        List.init board.Size (fun i ->
            let y = board.Size - 1 - i
            List.init board.Size (fun x ->
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
        List.init board.Size (fun i ->
            let y = board.Size - 1 - i
            List.init board.Size (fun x ->
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
          GameState = startingBoard.GameState()
          PlayerBlack = Human
          PlayerWhite = Computer Computer.Random.player }
    
    initialModel, Cmd.none

let updateBoard model board =
    { model with Board = board; PossibleMoves = board.PossibleMoves(); BoardView = toBoardView board; GameState = board.GameState() }

let requestComputerMove (player: ComputerPlayer, board) =
    async {
        do! Async.Sleep 1000
        return player.ChooseMove board
    }

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
        | Some possibleMove -> model, Cmd.ofMsg (GameAction (PlayMove possibleMove))
        | None -> model, Cmd.none

    | GameAction action ->
        let newModel =
            match action with
            | PlayMove possibleMove when List.exists (fun pm -> pm.MoveLocation = possibleMove.MoveLocation) model.PossibleMoves -> updateBoard model possibleMove.Result
            | SkipMove when model.GameState = OngoingSkipMove -> updateBoard model (model.Board.SkipMove())
            | _ -> model
        
        let computerRequest =
            match newModel.GameState, newModel.CurrentPlayer with
            | _, Human -> Cmd.none
            | Finished, _ -> Cmd.none
            | OngoingSkipMove, _ -> Cmd.ofMsg (GameAction SkipMove)
            | Ongoing, Computer player ->
                Cmd.OfAsync.perform requestComputerMove (player, newModel.Board) (fun move -> GameAction (PlayMove move))

        newModel, computerRequest
    
    | RestartGame ->
        if model.GameState = Finished then
            init()
        else
            model, Cmd.none
