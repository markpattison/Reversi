module FableReversi.TestComputer.Game

open FableReversi.Reversi
open FableReversi.Reversi.Computer.Players
open FableReversi.Reversi.Runner

type PlayerResult =
    {
        Player: ComputerPlayerChoice
        Wins: int
        Ties: int
        Losses: int
        PiecesFor: int
        PiecesAgainst: int
        Time: float
    }
    static member (+) (pr1: PlayerResult, pr2: PlayerResult) =
        if pr1.Player = pr2.Player then
            {
                Player = pr1.Player
                Wins = pr1.Wins + pr2.Wins
                Ties = pr1.Ties + pr2.Ties
                Losses = pr1.Losses + pr2.Losses
                PiecesFor = pr1.PiecesFor + pr2.PiecesFor
                PiecesAgainst = pr1.PiecesAgainst + pr2.PiecesAgainst
                Time = pr1.Time + pr2.Time
            }
        else failwith "Cannot aggregate results of different players"

let playerResultSummary pr =
    sprintf "%s: W %i, T %i, L %i  (avg. time %.2f)" pr.Player.Name pr.Wins pr.Ties pr.Losses (pr.Time / float (pr.Wins + pr.Ties + pr.Losses))

let oneIf condition =
    if condition then 1 else 0

type TimedResult =
    {
        Black: ComputerPlayerChoice
        White: ComputerPlayerChoice
        FinishedGame: FinishedGame
        TimeBlack: float
        TimeWhite: float
    }
    member this.PlayerResults =
        let numBlack = Bitwise.countStones this.FinishedGame.Board.BlackSquares
        let numWhite = Bitwise.countStones this.FinishedGame.Board.WhiteSquares
        [|
            { Player = this.Black
              Wins = oneIf (this.FinishedGame.Result = Win Black)
              Ties = oneIf (this.FinishedGame.Result = Tie)
              Losses = oneIf (this.FinishedGame.Result = Win White)
              PiecesFor = int numBlack
              PiecesAgainst = int numWhite
              Time = this.TimeBlack
            }
            { Player = this.White
              Wins = oneIf (this.FinishedGame.Result = Win White)
              Ties = oneIf (this.FinishedGame.Result = Tie)
              Losses = oneIf (this.FinishedGame.Result = Win Black)
              PiecesFor = int numWhite
              PiecesAgainst = int numBlack
              Time = this.TimeWhite
            }
        |]

let resultSummary result =
    let numBlack = Bitwise.countStones result.FinishedGame.Board.BlackSquares
    let numWhite = Bitwise.countStones result.FinishedGame.Board.WhiteSquares

    match result.FinishedGame.Result with
    | Tie -> sprintf "Tie! (%s: %.2fs, %s: %.2fs)" result.Black.Name result.TimeBlack result.White.Name result.TimeWhite
    | Win Black -> sprintf "%s beats %s %d-%d (%s: %.2fs, %s: %.2fs)" result.Black.Name result.White.Name numBlack numWhite result.Black.Name result.TimeBlack result.White.Name result.TimeWhite
    | Win White -> sprintf "%s beats %s %d-%d (%s: %.2fs, %s: %.2fs)" result.White.Name result.Black.Name numWhite numBlack result.White.Name result.TimeWhite result.Black.Name result.TimeBlack

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
