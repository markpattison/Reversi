module FableReversi.TestComputer.Series

open FableReversi.Reversi
open FableReversi.TestComputer.Game

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
