module FableReversi.TestComputer.Game

open FableReversi.Reversi
open FableReversi.Reversi.Runner

type TimedResult =
    {
        NameBlack: string
        NameWhite: string
        FinishedGame: FinishedGame
        TimeBlack: float
        TimeWhite: float
    }

let resultSummary result =
    match result.FinishedGame.Result with
    | Tie -> sprintf "Tie! (%s: %.2fs, %s: %.2fs)" result.NameBlack result.TimeBlack result.NameWhite result.TimeWhite
    | Win Black -> sprintf "%s beats %s %2i-%2i (%s: %.2fs, %s: %.2fs)" result.NameBlack result.NameWhite result.FinishedGame.Board.NumBlack result.FinishedGame.Board.NumWhite result.NameBlack result.TimeBlack result.NameWhite result.TimeWhite
    | Win White -> sprintf "%s beats %s %2i-%2i (%s: %.2fs, %s: %.2fs)" result.NameWhite result.NameBlack result.FinishedGame.Board.NumWhite result.FinishedGame.Board.NumBlack result.NameWhite result.TimeWhite result.NameBlack result.TimeBlack

let playGame playerBlackChoice playerWhiteChoice board =

    let playerBlack = Computer.Players.Create playerBlackChoice
    let playerWhite = Computer.Players.Create playerWhiteChoice

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

    let finishedGame = play board

    let gameResult =
          { NameBlack = playerBlackChoice.Name
            NameWhite = playerWhiteChoice.Name
            FinishedGame = finishedGame
            TimeWhite = float stopwatchWhite.ElapsedMilliseconds / 1000.0
            TimeBlack = float stopwatchBlack.ElapsedMilliseconds / 1000.0
          }
    
    printfn "%s" (resultSummary gameResult)

    gameResult
