namespace FableReversi.TestComputer

open NUnit.Framework

open FableReversi.Reversi
open FableReversi.Reversi.Runner

[<TestFixture>]
type TestComputer() = 

    let playGame playerBlack playerWhite board =
        let rec play b =
            let gameInfo = Board.toGameInfo b
            match gameInfo.State with
            | Finished f -> f
            | OngoingSkipMove _ -> Actions.skipMove gameInfo |> play
            | Ongoing ongoing ->
                let player = if ongoing.Board.NextToMove = Black then playerBlack else playerWhite
                let move = player.ChooseMove ongoing
                play (move.Result)

        play board

    [<Test>]
    member _.DummyTest() =
        
        let playerBlack = Computer.Random.create()
        let playerWhite = Computer.Heuristics.Basic.create 2

        for _ in 1..10 do
            let result = playGame playerBlack playerWhite Board.startingBoard

            let summary =
                match result.Result with
                | Tie -> "Tie!"
                | Win Black -> sprintf "Black wins %i-%i" result.Board.NumBlack result.Board.NumWhite
                | Win White -> sprintf "White wins %i-%i" result.Board.NumWhite result.Board.NumBlack

            printfn "%s" summary

        Assert.True(true)
