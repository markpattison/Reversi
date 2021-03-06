module FableReversi.Game.View

open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Reversi
open FableReversi.Reversi.Runner
open FableReversi.Shared.View

let toPieceIcon colour =
    [ Fa.i
        [ Fa.Size Fa.Fa2x
          Fa.Solid.Circle
          Fa.Props
            [ Style [ Color colour ] ] ]
        [] ]

let toCellProps colour : IHTMLProp list =
    [ Style
        [ TextAlign TextAlignOptions.Center
          VerticalAlign "middle"
          Height "50px"
          Width "50px"
          BackgroundColor colour ] ]

let blackPiece = toPieceIcon "#000000"
let whitePiece = toPieceIcon "#ffffff"
let blackFlipPiece = toPieceIcon "#606060"
let whiteFlipPiece = toPieceIcon "#d0d0d0"

let plainCellProps = toCellProps "#00b000"
let possibleMoveCellProps = toCellProps "#00d000"
let possibleMoveHoverCellProps = toCellProps "#00f000"
let wouldFlipCellProps = toCellProps "#00d000"

let showSquare dispatch humanPlaying (location, square:Square, view) =
    let cellProps : IHTMLProp list =
        match view with
        | Plain -> plainCellProps
        | PossibleMove -> possibleMoveCellProps
        | PossibleMoveHover -> possibleMoveHoverCellProps
        | WouldFlip -> wouldFlipCellProps

    let cellContent =
        match square, view with
        | Empty, _ -> []
        | Piece Black, WouldFlip -> blackFlipPiece
        | Piece Black, _ -> blackPiece
        | Piece White, WouldFlip -> whiteFlipPiece
        | Piece White, _ -> whitePiece

    let onHover = OnMouseOver (fun _ -> Hover location |> dispatch) :> IHTMLProp
    let onClick = OnClick (fun _ -> Click location |> dispatch) :> IHTMLProp

    let props = if humanPlaying then (onClick :: onHover :: cellProps) else cellProps

    td props cellContent

let showBoard dispatch humanPlaying boardView =
    let rows =
        boardView.SquareViews
        |> List.mapi (fun i row ->
            tr [] [
                td [] [str (string (i+1)) ]
                yield! row |> List.map (showSquare dispatch humanPlaying)
            ])
        |> List.append [ tr [] [ td [] []; for i in 0..7 do td [] [str (string (char (i + 65))) ] ]]

    Table.table
        [ Table.IsBordered
          Table.IsNarrow
          Table.Props
            [ Style [ TableLayout "fixed"; Height "400px"; Width "400px" ] ] ]
        [ tbody [] rows ]

let playerName player =
    match player with
    | Human -> "Human"
    | Computer c -> sprintf "Computer %s" (c.ToString())

let summary result =
    match result with
    | Win Black -> "Black wins!"
    | Win White -> "White wins!"
    | Tie -> "Game tied!"

let showDescription dispatch description =
    let rec showIndentedDescription indent desc =
        let subDescriptions =
            match desc.SubDescriptionsView with
            | Some (true, subs) -> subs |> Array.map (showIndentedDescription (indent + 1)) |> Array.toList
            | _ -> []
        let icon =
            match desc.SubDescriptionsView with
            | Some (true, _) -> [ Fa.Solid.CaretDown ]
            | Some (false, _) -> [ Fa.Solid.CaretRight ]
            | None -> []
        
        div []
            [ p [ OnClick (fun _ -> Expand desc.Id |> dispatch); Style [ PaddingLeft (sprintf "%iem" indent) ] ] [ Icon.icon [] [ Fa.i icon [] ]; desc.TextView |> str; br [] ]
              div [] subDescriptions ]
    
    div [] (description |> Array.map (showIndentedDescription 0) |> Array.toList)

let view model dispatch =
    let gameInfo = model.GameInfo

    let blackToPlay, whiteToPlay, result =
        match gameInfo.Board.NextToMove, gameInfo.State with
        | _, Finished fg -> false, false, Some fg.Result
        | Black, _ -> true, false, None
        | White, _ -> false, true, None

    let humanPlaying = match model.CurrentPlayer with | Human -> true | _ -> false

    let numBlack, numWhite = Board.countPieces gameInfo.Board
    
    let showSkipButton =
        match humanPlaying, gameInfo.State with
        | true, OngoingSkipMove _ -> true
        | _ -> false

    let showDiagnosticMode =
        match (snd model.PlayerBlack, snd model.PlayerWhite) with
        | Human, Human -> false
        | _ -> true
    
    let showDiagnostics = model.ComputerMoveWaiting.IsSome

    Columns.columns []
      [ Column.column [ Column.Width (Screen.All, Column.Is8) ]
          [ showBoard dispatch humanPlaying model.BoardView ]
        Column.column []
            [ p []
                    [ sprintf "Black (%s): %i " (fst model.PlayerBlack) numBlack |> str
                      if blackToPlay then Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              
              p []
                    [ sprintf "White (%s): %i " (fst model.PlayerWhite) numWhite |> str
                      if whiteToPlay then Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              
              if showDiagnostics then
                  br []
                  button "Play computer move" (fun _ -> dispatch (GameAction model.ComputerMoveWaiting.Value))
                  br []

              if showSkipButton then
                  br []
                  button "Skip move" (fun _ -> dispatch (GameAction SkipMove))
              
              if showDiagnosticMode then
                  br []
                  checkBox model.DiagnosticMode "show diagnostics" (fun _ -> dispatch ToggleDiagnosticMode)
              
              if showDiagnostics then
                  showDescription dispatch model.Diagnostics

              match result with
              | None -> ()
              | Some r ->
                  br []
                  p [] [ r |> summary |> str ]
                  br []
                  p [] [ button "Restart game" (fun _ -> dispatch Restart) ]
                  br[]
                  p [] [ button "Change players" (fun _ -> dispatch ChangePlayers) ] ] ]
