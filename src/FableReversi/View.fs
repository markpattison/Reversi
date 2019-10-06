module FableReversi.View

open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Reversi

let toPieceIcon colour = [ Fa.i [ Fa.Size Fa.Fa2x; Fa.Solid.Circle; Fa.Props [ Style [ Color colour ] ] ] [] ]
let toCellProps colour : IHTMLProp list = [ Style [ TextAlign TextAlignOptions.Center; VerticalAlign "middle"; Height "50px"; Width "50px"; BackgroundColor colour ] ]

let blackPiece = toPieceIcon "#000000"
let whitePiece = toPieceIcon "#ffffff"
let blackFlipPiece = toPieceIcon "#606060"
let whiteFlipPiece = toPieceIcon "#d0d0d0"

let plainCellProps = toCellProps "#00b000"
let possibleMoveCellProps = toCellProps "#00d000"
let possibleMoveHoverCellProps = toCellProps "#00f000"
let wouldFlipCellProps = toCellProps "#00d000"

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let showSquare dispatch humanPlaying (location, square, view) =
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
        |> List.map (fun row -> 
            tr [] (row |> List.map (showSquare dispatch humanPlaying)))
    
    Table.table [ Table.IsBordered; Table.IsNarrow; Table.Props [ Style [ TableLayout "fixed"; Height "400px"; Width "400px" ] ] ]
        [ tbody [] rows ]


let content (model : Model) (dispatch : Msg -> unit) =
    let humanPlaying = match model.CurrentPlayer with | Human -> true | _ -> false
    let blackToPlay = model.Board.NextToMove = Black && model.GameState <> Finished
    let whiteToPlay = model.Board.NextToMove = White && model.GameState <> Finished

    Columns.columns []
      [ Column.column [ Column.Width (Screen.All, Column.Is9) ]
          [ showBoard dispatch humanPlaying model.BoardView ]
        Column.column []
            [ yield
                p []
                    [ yield sprintf "Black: %i " (model.Board.NumPieces Black) |> str
                      if blackToPlay then yield Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              yield
                p []
                    [ yield sprintf "White: %i " (model.Board.NumPieces White) |> str
                      if whiteToPlay then yield Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              yield br []
              if humanPlaying && model.GameState = OngoingSkipMove then yield button "Skip move" (fun _ -> dispatch (GameAction SkipMove))
              if model.GameState = Finished then yield button "Restart game" (fun _ -> dispatch RestartGame) ] ]

let view (model : Model) (dispatch : Msg -> unit) =

    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Fable Reversi" ] ] ]

          Section.section []
                [ Container.container []
                    [ content model dispatch ] ] ]

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram State.init State.update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withDebugger
#endif
|> Program.run