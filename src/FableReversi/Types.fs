module FableReversi.Types

open FableReversi.Reversi

type Model = { Board: Board }

type Msg =
    | Increment
    | Decrement
