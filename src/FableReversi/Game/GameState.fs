module FableReversi.Game.State

open Elmish

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open FableReversi.Lobby.Types
open Types

let getPossibleMoves gameInfo =
    match gameInfo.State with
    | Ongoing { PossibleMoves = moves } -> moves
    | _ -> [||]

let getPossibleMoveLocations gameInfo =
    getPossibleMoves gameInfo
    |> Array.map (fun pm -> pm.Pos)

let toBoardView gameInfo =
    let possibleMoveLocations = getPossibleMoveLocations gameInfo

    { SquareViews =
        List.init 8 (fun i ->
            let y = 7 - i
            List.init 8 (fun x ->
                let location = Bitboard.pos x y
                let view =
                    if Array.contains location possibleMoveLocations then
                        PossibleMove
                    else
                        Plain
                let square =
                    if Bitboard.isSet location gameInfo.Board.BlackSquares then
                        Piece Black
                    elif Bitboard.isSet location gameInfo.Board.WhiteSquares then
                        Piece White
                    else
                        Empty

                ((x,y), square, view))) }

let toBoardViewPossibleMoveHover gameInfo possibleMove =
    let possibleMoveLocations = getPossibleMoveLocations gameInfo

    { SquareViews =
        List.init 8 (fun i ->
            let y = 7 - i
            List.init 8 (fun x ->
                let location = Bitboard.pos x y
                let view =
                    if possibleMove.Pos = location then
                        PossibleMoveHover
                    elif Bitboard.isSet location possibleMove.Flips then
                        WouldFlip
                    elif Array.contains location possibleMoveLocations then
                        PossibleMove
                    else
                        Plain
                let square =
                    if Bitboard.isSet location gameInfo.Board.BlackSquares then
                        Piece Black
                    elif Bitboard.isSet location gameInfo.Board.WhiteSquares then
                        Piece White
                    else
                        Empty
                ((x,y), square, view))) }

let rec toDescriptionView uniqueId description =
    match description.SubDescriptions with
    | [||] ->
        uniqueId := !uniqueId + 1
        { Id = !uniqueId; TextView = description.Text; SubDescriptionsView = None }
    | sd ->
        uniqueId := !uniqueId + 1
        { Id = !uniqueId; TextView = description.Text; SubDescriptionsView = Some (false, sd |> Array.map (toDescriptionView uniqueId)) }

let createPlayer playerChoice =
    match playerChoice with
    | HumanChoice -> "Human", Human
    | ComputerChoice c -> sprintf "Computer %A" c, Computer (Computer.Players.create c)

let newGame blackPlayer whitePlayer =
    let startingBoard = Board.startingBoard
    let gameInfo = Board.toGameInfo startingBoard
    let black = createPlayer blackPlayer
    let white = createPlayer whitePlayer
    let uniqueId = ref 0

    { GameInfo = gameInfo
      BoardView = toBoardView gameInfo
      PlayerBlackChoice = blackPlayer
      PlayerWhiteChoice = whitePlayer
      PlayerBlack = black
      PlayerWhite = white
      Diagnostics = [||]
      DiagnosticMode = false
      ComputerMoveWaiting = None
      UniqueId = uniqueId }

let updateBoard model board =
    let gameInfo = Board.toGameInfo board
    { model with GameInfo = gameInfo; BoardView = toBoardView gameInfo }

let requestComputerMove (player: ComputerPlayer, board) =
    async {
        do! Async.Sleep 100
        return player.ChooseMove board
    }

let rec toggleExpanded descId descView =
    { descView with SubDescriptionsView =
                        match descView.SubDescriptionsView with
                        | None -> None
                        | Some (expanded, subs) when descId = descView.Id -> Some (not expanded, subs)
                        | Some (expanded, subs) -> Some (expanded, subs |> Array.map (toggleExpanded descId)) }

let update (msg : GameMsg) (model : GameModel) : GameModel * Cmd<GameMsg> =
    let possibleMoves = getPossibleMoves model.GameInfo

    match msg with

    | Hover (x,y) ->
        let location = Bitboard.pos x y
        match Array.tryFind (fun possibleMove -> possibleMove.Pos = location) possibleMoves with
        | Some possibleMove ->
            let boardView = toBoardViewPossibleMoveHover model.GameInfo possibleMove
            { model with BoardView = boardView }, Cmd.none

        | None -> { model with BoardView = toBoardView model.GameInfo }, Cmd.none

    | Click (x,y) ->
        let location = Bitboard.pos x y
        match Array.tryFind (fun possibleMove -> possibleMove.Pos = location) possibleMoves with
        | Some possibleMove -> model, Cmd.ofMsg (GameAction (PlayMove possibleMove))
        | None -> model, Cmd.none

    | ComputerActionReady action ->
        let nextToMove = model.GameInfo.Board.NextToMove
        if model.DiagnosticMode then
            let cmd =
                match action with
                | SkipMove -> Cmd.none
                | PlayMove possibleMove ->
                    let posXY = Bitboard.getXY possibleMove.Pos
                    Cmd.ofMsg (Hover posXY)
            { model with
                ComputerMoveWaiting = Some action
                Diagnostics =
                    match nextToMove with
                    | Black -> (model.PlayerBlack |> snd).Describe() |> Array.map (toDescriptionView model.UniqueId)
                    | White -> (model.PlayerWhite |> snd).Describe() |> Array.map (toDescriptionView model.UniqueId)
            }, cmd
        else
            model, Cmd.ofMsg (GameAction action)
    
    | GameAction action ->
        let newModel =
            match action, model.GameInfo.State with
            | PlayMove possibleMove, Ongoing _ when Array.exists (fun pm -> pm.Pos = possibleMove.Pos) possibleMoves ->
                if model.GameInfo.Board.NextToMove = White then
                    match model.PlayerBlack with
                    | _, Computer p -> p.OpponentSelected possibleMove
                    | _ -> ()
                else
                    match model.PlayerWhite with
                    | _, Computer p -> p.OpponentSelected possibleMove
                    | _ -> ()

                updateBoard model possibleMove.Result

            | SkipMove, OngoingSkipMove _ ->
                match model.PlayerBlack with
                | _, Computer p -> p.OnMoveSkipped()
                | _ -> ()
                match model.PlayerWhite with
                | _, Computer p -> p.OnMoveSkipped()
                | _ -> ()

                updateBoard model (Actions.skipMove model.GameInfo)

            | _ -> model

        { newModel with ComputerMoveWaiting = None }, Cmd.ofMsg RequestComputerMoveIfNeeded

    | RequestComputerMoveIfNeeded ->
        match model.GameInfo.State, model.CurrentPlayer with
        | _, Human -> model, Cmd.none
        | Finished _, _ -> model, Cmd.none
        | OngoingSkipMove _, _ -> model, Cmd.ofMsg (GameAction SkipMove)
        | Ongoing ongoingGame, Computer player ->
            model, Cmd.OfAsync.perform requestComputerMove (player, ongoingGame) (PlayMove >> ComputerActionReady)

    | ToggleDiagnosticMode ->
        { model with DiagnosticMode = not model.DiagnosticMode }, Cmd.none

    | Expand descId ->
        { model with
            Diagnostics = model.Diagnostics |> Array.map (toggleExpanded descId) }, Cmd.none

    | Restart -> model, Cmd.none // handled at Model level
    | ChangePlayers -> model, Cmd.none // handled at Model level
