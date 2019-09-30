module FableReversi.View

open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Reversi

let blackColour = Fa.Props [ Style [ Color "black" ] ]
let whiteColour = Fa.Props [ Style [ Color "white" ] ]

let tableCellProps : IHTMLProp list = [ Style [ TextAlign TextAlignOptions.Center; VerticalAlign "middle"; Height "50px"; Width "50px" ] ]

let showSquare square =

    match square with
    | Empty -> td tableCellProps []
    | Piece Black -> td tableCellProps [ Fa.i [ Fa.Size Fa.Fa2x; Fa.Solid.Circle; blackColour ] [] ]
    | Piece White -> td tableCellProps [ Fa.i [ Fa.Size Fa.Fa2x; Fa.Solid.Circle; whiteColour ] [] ]

let showBoard board =
    let rows =
        [ for y in (board.Size - 1).. -1 ..0 do
            yield tr []
                [ for x in 0..(board.Size - 1) do
                    yield showSquare (Board.pieceAt board (Location (x, y))) ] ]
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
              [ showBoard model.Board ] ]
