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

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    let humanPlaying = match model.CurrentPlayer with | Human -> true | _ -> false

    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Fable Reversi" ] ] ]

          Container.container []
              [ yield showBoard dispatch humanPlaying model.BoardView
                yield p [] [ sprintf "Next to play: %O" model.Board.NextToMove |> str ]
                yield p [] [ sprintf "State: %O" (model.Board.GameState()) |> str ]
                
                if model.GameState = OngoingSkipMove then yield button "Skip move" (fun _ -> dispatch (GameAction SkipMove))
                if model.GameState = Finished then yield button "Restart game" (fun _ -> dispatch RestartGame) ] ]
