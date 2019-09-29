module FableReversi.State

open Elmish

open Types

let init () =
    let initialModel = { Counter = 0 }
    initialModel, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | n, Increment ->
        let nextModel = { currentModel with Counter = n + 1 }
        nextModel, Cmd.none
    | n, Decrement ->
        let nextModel = { currentModel with Counter = n - 1 }
        nextModel, Cmd.none
