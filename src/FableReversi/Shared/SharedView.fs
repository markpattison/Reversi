module FableReversi.Shared.View

open Fable.React
open Fulma

let button txt onClick =
    Button.button
        [ Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]
