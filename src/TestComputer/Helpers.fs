module FableReversi.TestComputer.Helpers

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

type SeriesResult =
    {
        NameOne: string
        NameTwo: string
        WinsOne: int
        WinsTwo: int
        Ties: int
        PiecesOne: int
        PiecesTwo: int
        AverageTimeOne: float
        AverageTimeTwo: float
    }

let seriesSummary series =
    sprintf "\n%s (avg. time %.2f): %i, ties: %i, %s (avg. time %.2f): %i\n" series.NameOne series.AverageTimeOne series.WinsOne series.Ties series.NameTwo series.AverageTimeTwo series.WinsTwo

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

let playSeries playerOneChoice playerTwoChoice gamesPerSide =
    let resultsOneAsBlack =
        Array.init gamesPerSide (fun _ -> playGame playerOneChoice playerTwoChoice Board.startingBoard)
    let resultsTwoAsBlack =
        Array.init gamesPerSide (fun _ -> playGame playerTwoChoice playerOneChoice Board.startingBoard)

    let numGames = float (resultsOneAsBlack.Length + resultsTwoAsBlack.Length)

    let seriesResult =
      { NameOne = playerOneChoice.Name
        NameTwo = playerTwoChoice.Name
        WinsOne =
            (resultsOneAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Win Black) |> Seq.length) +
            (resultsTwoAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Win White) |> Seq.length)
        WinsTwo =
            (resultsOneAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Win White) |> Seq.length) +
            (resultsTwoAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Win Black) |> Seq.length)
        Ties =
            (resultsOneAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Tie) |> Seq.length) +
            (resultsTwoAsBlack |> Seq.where (fun r -> r.FinishedGame.Result = Tie) |> Seq.length)
        PiecesOne =
            (resultsOneAsBlack |> Seq.sumBy (fun r -> r.FinishedGame.Board.NumBlack)) +
            (resultsTwoAsBlack |> Seq.sumBy (fun r -> r.FinishedGame.Board.NumWhite))
        PiecesTwo =
            (resultsOneAsBlack |> Seq.sumBy (fun r -> r.FinishedGame.Board.NumWhite)) +
            (resultsTwoAsBlack |> Seq.sumBy (fun r -> r.FinishedGame.Board.NumBlack))
        AverageTimeOne =
            ((resultsOneAsBlack |> Seq.sumBy (fun r -> r.TimeBlack)) +
             (resultsTwoAsBlack |> Seq.sumBy (fun r -> r.TimeWhite))) / numGames
        AverageTimeTwo =
            ((resultsOneAsBlack |> Seq.sumBy (fun r -> r.TimeWhite)) +
             (resultsTwoAsBlack |> Seq.sumBy (fun r -> r.TimeBlack))) / numGames
      }
    
    printfn "%s" (seriesSummary seriesResult)
    
    seriesResult
