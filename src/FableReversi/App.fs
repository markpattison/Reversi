module FableReversi.App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma

open Types

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram State.init State.update View.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withDebugger
#endif
|> Program.run
