module FableReversi.TestComputer.TestBoard

open Expecto
open FableReversi.Reversi.Board
open FableReversi.Reversi

let allTests =
    testList "Boards and Moves" [
        testCase "init board" (fun _ ->
            let board = startingBoard
            let moves = getPossibleMoves board
            Expect.equal moves.Length 4 "4 moves at start"

            let numBlack, numWhite = countPieces board

            Expect.equal numBlack 2 "2 black stones at start"
            Expect.equal numWhite 2 "2 white stones at start"
        )

        testCase "first 2 moves" (fun _ ->
            let board = startingBoard

            let moves = getPossibleMoves board
            let pos = moves.[0]
            let board = (applyMove pos board).Result
            let moves = getPossibleMoves board
            Expect.equal moves.Length 3 "3 moves for white"

            let numBlack, numWhite = countPieces board

            Expect.equal numBlack 4 "4 black stones after first move"
            Expect.equal numWhite 1 "1 white stone after first move"

            for pos in moves do
                let move = applyMove pos board
                let board = move.Result
                let numBlack, numWhite = countPieces board

                Expect.equal numBlack 3 "3 black stones after second move"
                Expect.equal numWhite 3 "3 white stones after second move"
        )

        testCase "count gives same results as slowCount" (fun _ ->
            let random = System.Random(27)
            let getNextUint64 () =
                let buffer : byte[] = Array.zeroCreate sizeof<uint64>
                random.NextBytes(buffer)
                System.BitConverter.ToUInt64(buffer, 0)
            
            for _ in 1 .. 10000 do
                let bitboard : Bitboard = getNextUint64()
                let countResult = Bitboard.count bitboard
                let slowCountresult = Bitboard.slowCount (bitboard)
                Expect.equal countResult slowCountresult (sprintf "count and slowCount give different results for %i" bitboard)
        )

        testCase "count is faster than slowCount" (fun _ ->
            let random = System.Random(27)
            let getNextUint64 () =
                let buffer : byte[] = Array.zeroCreate sizeof<uint64>
                random.NextBytes(buffer)
                System.BitConverter.ToUInt64(buffer, 0)
            
            let tryF f =
                for _ in 1 .. 10000 do
                    let bitboard : Bitboard = getNextUint64()
                    bitboard |> f |> ignore

            Expect.isFasterThan (fun _ -> tryF Bitboard.count)
                                (fun _ -> tryF Bitboard.slowCount)
                                "count should be faster than slowCount"
        )
    ]