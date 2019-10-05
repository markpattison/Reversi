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

let showSquare dispatch (location, square, view) =
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

    td (onClick :: onHover :: cellProps) cellContent

let showBoard dispatch boardView =
    let rows =
        boardView.SquareViews
        |> List.map (fun row -> 
            tr [] (row |> List.map (showSquare dispatch)))
    
    Table.table [ Table.IsBordered; Table.IsNarrow; Table.Props [ Style [ TableLayout "fixed"; Height "400px"; Width "400px" ] ] ]
        [ tbody [] rows ]

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Fable Reversi" ] ] ]

          Container.container []
              [ showBoard dispatch model.BoardView
                sprintf "Next to play: %O" model.Board.NextToMove |> str ] ]
