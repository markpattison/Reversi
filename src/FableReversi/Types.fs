module FableReversi.Types

open FableReversi.Reversi

type Model = { Position: Position }

type Msg =
    | Increment
    | Decrement
