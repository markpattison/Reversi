module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma

type Model = { Counter: int }

type Msg =
    | Increment
    | Decrement


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

let show = function
    | { Counter = n } -> string n

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
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
                      Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
