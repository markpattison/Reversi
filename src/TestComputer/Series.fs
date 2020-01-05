module FableReversi.TestComputer.Series

open FableReversi.Reversi
open FableReversi.Reversi.Computer.Players
open FableReversi.TestComputer.Game

type SeriesResult =
    {
        PlayerOne: ComputerPlayerChoice
        PlayerTwo: ComputerPlayerChoice
        WinsOne: int
        WinsTwo: int
        Ties: int
        PiecesOne: int
        PiecesTwo: int
        AverageTimeOne: float
        AverageTimeTwo: float
    }

let seriesSummary series =
    sprintf "\n%s (avg. time %.2f): %i, ties: %i, %s (avg. time %.2f): %i\n" series.PlayerOne.Name series.AverageTimeOne series.WinsOne series.Ties series.PlayerTwo.Name series.AverageTimeTwo series.WinsTwo

let playSeries playerOne playerTwo gamesPerSide =
    let resultsOneAsBlack =
        Array.init gamesPerSide (fun _ -> playGame playerOne playerTwo)
    let resultsTwoAsBlack =
        Array.init gamesPerSide (fun _ -> playGame playerTwo playerOne)

    let numGames = float (resultsOneAsBlack.Length + resultsTwoAsBlack.Length)

    let seriesResult =
      { PlayerOne = playerOne
        PlayerTwo = playerTwo
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
