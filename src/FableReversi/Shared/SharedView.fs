module FableReversi.Shared.View

open Fable.React
open Fulma

let button txt onClick =
    Button.button
        [ Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let checkBox isChecked txt onClick =
    Checkbox.checkbox [ Props [ Props.OnClick onClick ] ]
        [ Checkbox.input [ Props [ Props.Checked isChecked ] ]
          str txt ]
