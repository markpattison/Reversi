module FableReversi.Reversi.Computer.Greedy

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let create() =
    let random = System.Random()
    let mutable lastDescription = [||]

    {
        OpponentSelected = ignore
        OnMoveSkipped = ignore
        Describe = fun () -> lastDescription

        ChooseMove = fun ongoingGame ->
            let movesWithFlips =
                ongoingGame.PossibleMoves
                |> Array.groupBy (fun pm -> Bitboard.count pm.Flips)

            let movesWithMostFlips =
                movesWithFlips
                |> Array.maxBy fst
                |> snd

            let choice = random.Next(0, movesWithMostFlips.Length)
            let selected = movesWithMostFlips.[choice]

            lastDescription <-
                movesWithFlips
                |> Array.sortByDescending fst
                |> Array.collect (fun (score,moves) ->
                    moves
                    |> Array.sortByDescending (fun m -> if m = selected then 1 else 0)
                    |> Array.map (fun move -> { Text = Bitboard.printPos move.Pos + " => " + string score; SubDescriptions = [||] } )
                )

            selected
    }
