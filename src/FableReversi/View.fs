module FableReversi.View

open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Reversi

let blackColour = Fa.Props [ Style [ Color "black" ] ]
let whiteColour = Fa.Props [ Style [ Color "white" ] ]

let plainCellProps : IHTMLProp list = [ Style [ TextAlign TextAlignOptions.Center; VerticalAlign "middle"; Height "50px"; Width "50px"; BackgroundColor "#00b000" ] ]
let possibleMoveCellProps : IHTMLProp list = [ Style [ TextAlign TextAlignOptions.Center; VerticalAlign "middle"; Height "50px"; Width "50px"; BackgroundColor "#00f000" ] ]
let wouldFlipCellProps : IHTMLProp list = [ Style [ TextAlign TextAlignOptions.Center; VerticalAlign "middle"; Height "50px"; Width "50px"; BackgroundColor "#00d000" ] ]

let showSquare dispatch location (square, view) =
    let cellProps : IHTMLProp list =
        match view with
        | Plain -> plainCellProps
        | PossibleMove -> possibleMoveCellProps
        | WouldFlip -> wouldFlipCellProps

    let cellContent =
        match square with
        | Empty -> []
        | Piece Black -> [ Fa.i [ Fa.Size Fa.Fa2x; Fa.Solid.Circle; blackColour ] [] ]
        | Piece White -> [ Fa.i [ Fa.Size Fa.Fa2x; Fa.Solid.Circle; whiteColour ] [] ]
    
    let onHover = OnMouseOver (fun _ -> Hover location |> dispatch) :> IHTMLProp

    td (onHover :: cellProps) cellContent

let showBoard dispatch boardView =
    let rows =
        [ for y in (boardView.SizeView - 1).. -1 ..0 do
            yield tr []
                [ for x in 0..(boardView.SizeView - 1) do
                    let location = Location (x, y)
                    yield showSquare dispatch location (boardView.PieceAt(location)) ] ]
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
              [ showBoard dispatch model.BoardView ] ]
