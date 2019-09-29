module FableReversi.View

open Fable.React
open Fulma

open Types
open FableReversi.Reversi

let showSquare square =
    match square with
    | Empty -> str "#"
    | Piece Black -> str "B"
    | Piece White -> str "W"

let showBoard position =
    let rows =
        [ for y in (position.Size - 1).. -1 ..0 do
            yield tr []
                [ for x in 0..(position.Size - 1) do
                    yield showSquare (Position.pieceAt position (Location (x, y)) ) ] ]
    Table.table [ Table.IsBordered; Table.IsNarrow ]
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
              [ showBoard model.Position
              ] ]
