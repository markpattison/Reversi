module FableReversi.Reversi.Computer.Logged

open FableReversi.Reversi.Runner

type PlayerLog =
    {
        NewGame: unit -> unit
        TotalTime: unit -> float
        MaxMoveTime: unit -> float
    }

let create player =
    let stopwatch = System.Diagnostics.Stopwatch()
    let mutable maxMoveTime = 0L
    let timedPlayer =
        {
            ChooseMove = fun ongoingGame ->
                let timeBeforeMove = stopwatch.ElapsedMilliseconds
                stopwatch.Start()
                let choice = player.ChooseMove ongoingGame
                stopwatch.Stop()
                if stopwatch.ElapsedMilliseconds - timeBeforeMove > maxMoveTime then maxMoveTime <- stopwatch.ElapsedMilliseconds - timeBeforeMove
                choice
        }

    let log =
        {
            NewGame = fun () -> stopwatch.Reset(); maxMoveTime <- 0L
            TotalTime = fun () -> (float stopwatch.ElapsedMilliseconds) / 1000.0
            MaxMoveTime = fun () -> (float maxMoveTime) / 1000.0
        }

    (timedPlayer, log)
