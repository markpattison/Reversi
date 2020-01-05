module FableReversi.TestComputer.Game

open FableReversi.Reversi
open FableReversi.Reversi.Computer.Players
open FableReversi.Reversi.Runner

type TimedResult =
    {
        Black: ComputerPlayerChoice
        White: ComputerPlayerChoice
        FinishedGame: FinishedGame
        TimeBlack: float
        TimeWhite: float
    }

let resultSummary result =
    match result.FinishedGame.Result with
    | Tie -> sprintf "Tie! (%s: %.2fs, %s: %.2fs)" result.Black.Name result.TimeBlack result.White.Name result.TimeWhite
    | Win Black -> sprintf "%s beats %s %2i-%2i (%s: %.2fs, %s: %.2fs)" result.Black.Name result.White.Name result.FinishedGame.Board.NumBlack result.FinishedGame.Board.NumWhite result.Black.Name result.TimeBlack result.White.Name result.TimeWhite
    | Win White -> sprintf "%s beats %s %2i-%2i (%s: %.2fs, %s: %.2fs)" result.White.Name result.Black.Name result.FinishedGame.Board.NumWhite result.FinishedGame.Board.NumBlack result.White.Name result.TimeWhite result.Black.Name result.TimeBlack

let playGame black white =

    let playerBlack = create black
    let playerWhite = create white

    let stopwatchWhite = System.Diagnostics.Stopwatch()
    let stopwatchBlack = System.Diagnostics.Stopwatch()

    let rec play b =
        let gameInfo = Board.toGameInfo b
        match gameInfo.State with
        | Finished f -> f
        | OngoingSkipMove _ ->
            playerWhite.OnMoveSkipped()
            playerBlack.OnMoveSkipped()
            Actions.skipMove gameInfo |> play
        | Ongoing ongoing ->

            if ongoing.Board.NextToMove = Black then
                stopwatchBlack.Start()
                let move = playerBlack.ChooseMove ongoing
                stopwatchBlack.Stop()
                playerWhite.OpponentSelected move
                play (move.Result)
            else
                stopwatchWhite.Start()
                let move = playerWhite.ChooseMove ongoing
                stopwatchWhite.Stop()
                playerBlack.OpponentSelected move
                play (move.Result)

    let finishedGame = play Board.startingBoard

    let gameResult =
          { Black = black
            White = white
            FinishedGame = finishedGame
            TimeWhite = float stopwatchWhite.ElapsedMilliseconds / 1000.0
            TimeBlack = float stopwatchBlack.ElapsedMilliseconds / 1000.0
          }
    
    printfn "%s" (resultSummary gameResult)

    gameResult
