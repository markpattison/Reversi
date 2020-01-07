module FableReversi.TestComputer.TestBoard

open Expecto
open FableReversi.Reversi.Board
open FableReversi.Reversi
open FableReversi.Reversi.Bitwise

let allTests =
    testList "Boards and Moves" [
        testCase "init board" (fun _ ->
            let board = startingBoard
            let moves = getPossibleMoves board
            Expect.equal moves.Length 4 "4 moves at start"

            let numBlack = countStones board.BlackSquares
            let numWhite = countStones board.WhiteSquares

            Expect.equal numBlack 2UL "2 black stones at start"
            Expect.equal numWhite 2UL "2 white stones at start"
        )

        testCase "first 2 moves" (fun _ ->
            let board = startingBoard

            let moves = getPossibleMoves board
            let pos = moves.[0]
            let board = (applyMove pos board).Result
            let moves = getPossibleMoves board
            Expect.equal moves.Length 3 "3 moves for white"

            let numBlack = countStones board.BlackSquares
            let numWhite = countStones board.WhiteSquares

            Expect.equal numBlack 4UL "4 black stones after first move"
            Expect.equal numWhite 1UL "1 white stone after first move"

            for pos in moves do
                let move = applyMove pos board
                let board = move.Result
                let numBlack = countStones board.BlackSquares
                let numWhite = countStones board.WhiteSquares

                Expect.equal numBlack 3UL "3 black stones after second move"
                Expect.equal numWhite 3UL "3 white stones after second move"
        )
    ]