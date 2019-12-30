module FableReversi.TestComputer.Helpers

open FableReversi.Reversi
open FableReversi.Reversi.Runner

type TimedResult =
    {
        FinishedGame: FinishedGame
        TimeWhite: float
        TimeBlack: float
    }
    override this.ToString() =
        let times = sprintf "White: %.2fs, Black %.2fs" this.TimeWhite this.TimeBlack
        match this.FinishedGame.Result with
        | Tie -> sprintf "Tie! (%s)" times
        | Win Black -> sprintf "Black wins %i-%i (%s)" this.FinishedGame.Board.NumBlack this.FinishedGame.Board.NumWhite times
        | Win White -> sprintf "White wins %i-%i (%s)" this.FinishedGame.Board.NumWhite this.FinishedGame.Board.NumBlack times

let playGame createPlayerBlack createPlayerWhite board =

    let playerBlack = createPlayerBlack ()
    let playerWhite = createPlayerWhite ()

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

    {
        FinishedGame = finishedGame
        TimeWhite = float stopwatchWhite.ElapsedMilliseconds / 1000.0
        TimeBlack = float stopwatchBlack.ElapsedMilliseconds / 1000.0
    }