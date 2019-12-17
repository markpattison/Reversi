module FableReversi.TestComputer.Helpers

open FableReversi.Reversi
open FableReversi.Reversi.Runner


let playGame playerBlack playerWhite board =
    let rec play b =
        let gameInfo = Board.toGameInfo b
        match gameInfo.State with
        | Finished f -> f
        | OngoingSkipMove _ ->
            playerWhite.OnMoveSkipped()
            playerBlack.OnMoveSkipped()
            Actions.skipMove gameInfo |> play
        | Ongoing ongoing ->
            let player,opponent = if ongoing.Board.NextToMove = Black then playerBlack,playerWhite else playerWhite,playerBlack
            let move = player.ChooseMove ongoing
            opponent.OpponentSelected move
            play (move.Result)

    play board