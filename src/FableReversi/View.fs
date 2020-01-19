module FableReversi.View

open Fable.React
open Fulma

open Types

let content model dispatch =
    match model.OuterState with
    | Lobby lobbyModel -> Lobby.View.view lobbyModel (LobbyMsg >> dispatch)
    | Playing gameModel -> Game.View.view gameModel (GameMsg >> dispatch)

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Fable Reversi" ] ] ]

          Section.section []
                [ Container.container []
                    [ content model dispatch ] ] ]
