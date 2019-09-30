module FableReversi.State

open Elmish

open Types

let init () =
    let initialModel = { Board = Reversi.Board.createStarting() }
    initialModel, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match model, msg with
    | _ ->
        model, Cmd.none
