namespace FableReversi.Reversi

type Bitboard = uint64

module Bitboard =
    let empty: Bitboard = 0UL

    let inline pos (x:int) (y:int) = x + 8 * y

    let inline getXY (pos:int) = pos % 8, pos / 8

    let inline set (pos:int) (board:Bitboard) =
        board ||| (1UL <<< pos)

    let inline unset (pos:int) (board:Bitboard) =
        board &&& (~~~ (1UL <<< pos))

    let inline isSet (pos:int) (board:Bitboard) =
        ((board >>> pos ) &&& 1UL) = 1UL

    let slowCount (board:Bitboard) =
        let mutable count = 0
        let mutable board = board
        while board > 0UL do
            count <- count + int (board &&& 1UL)
            board <- board >>> 1
        count

    let private precalculatedBitCounts =
        Array.init 256 (fun n ->
            let mutable count = 0
            let mutable n' = n
            while n' > 0 do
                count <- count + int (n' &&& 1)
                n' <- n' >>> 1
            count)

    let count (board:Bitboard) =
        precalculatedBitCounts.[int (board &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 8) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 16) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 24) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 32) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 40) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 48) &&& 255UL)]
        + precalculatedBitCounts.[int ((board >>> 56) &&& 255UL)]

    let printPos (p:int) =
        let x,y = getXY p
        sprintf "%c%i" (char (65 + x)) (8-y)

[<Struct>]
type Colour =
    | Black
    | White
    member this.Opposite =
        match this with
        | Black -> White
        | White -> Black

type GameResult =
    | Win of Colour
    | Tie

type Board =
    {
        WhiteSquares: Bitboard
        BlackSquares: Bitboard
        NextToMove: Colour
    }

    with
        override this.ToString() =
            let mutable s = "   "

            for y in 0..7 do
                s <- s + " " + string y
            s <- s + "\n"

            for x in 0..7 do
                s <- s + " " + string x + " "
                for y in 0..7 do
                    let pos = Bitboard.pos x y
                    if Bitboard.isSet pos this.BlackSquares then
                        s <- s + " B"
                    elif Bitboard.isSet pos this.WhiteSquares then
                        s <- s + " W"
                    else
                        s <- s + "  "
                s <- s + "\n"
            s

type PossibleMove =
    {
        Pos: int
        Flips: Bitboard
        Result: Board
    }

    with
        override this.ToString() =
            let mutable s = "   "

            for y in 0..7 do
                s <- s + " " + string y
            s <- s + "\n"

            for x in 0..7 do
                s <- s + " " + string x + " "
                for y in 0..7 do
                    let pos = Bitboard.pos x y
                    if this.Pos = pos then
                        s <- s + " *"
                    else
                        let pos = Bitboard.pos x y
                        if Bitboard.isSet pos this.Result.BlackSquares then
                            s <- s + " B"
                        elif Bitboard.isSet pos this.Result.WhiteSquares then
                            s <- s + " W"
                        else
                            s <- s + "  "

                s <- s + "\n"
            s

type OngoingGame =
    {
        Board: Board
        PossibleMoves: PossibleMove []
    }

type FinishedGame =
    {
        Board: Board
        Result: GameResult
    }

type GameState =
    | Ongoing of OngoingGame
    | OngoingSkipMove of Board
    | Finished of FinishedGame

type GameInfo =
    {
        State: GameState
    }
    member this.Board =
        match this.State with
        | Ongoing og -> og.Board
        | OngoingSkipMove b -> b
        | Finished fg -> fg.Board

module Board =

    let private calculateDirectionsAndMaxSquares pos =
        let x, y = Bitboard.getXY pos
        let allDirections =
            [| ( 1,  0, 7 - x)
               ( 1,  1, min (7 - x) (7 - y))
               ( 0,  1, 7 - y)
               (-1,  1, min x (7 - y))
               (-1,  0, x)
               (-1, -1, min x y)
               ( 0, -1, y)
               ( 1, -1, min (7 - x) y)
            |]

        allDirections
        |> Array.filter (fun (_, _, maxSquares) -> maxSquares >= 2)
        |> Array.map (fun (dx, dy, maxSquares) -> (Bitboard.pos dx dy, maxSquares))

    let private preCalculatedDirectionsAndMaxSquares =
        Array.init 64 calculateDirectionsAndMaxSquares

    let startingBoard = {
        WhiteSquares =
            Bitboard.empty
            |> Bitboard.set (Bitboard.pos 4 3)
            |> Bitboard.set (Bitboard.pos 3 4)

        BlackSquares =
            Bitboard.empty
            |> Bitboard.set (Bitboard.pos 3 3)
            |> Bitboard.set (Bitboard.pos 4 4)

        NextToMove = Black
    }

    let countPieces board =
        Bitboard.count board.BlackSquares, Bitboard.count board.WhiteSquares

    let private findFlips (board:Board) colour pos (direction, maxSquares) =
        let mutable squaresToTry = maxSquares

        let mutable tryPos = pos + direction
        let mutable foundEmpty = false
        let mutable foundMyColour = false
        let mutable flips = Bitboard.empty

        let mySquares, oppSquares =
            match colour with
            | Black -> board.BlackSquares, board.WhiteSquares
            | White -> board.WhiteSquares, board.BlackSquares

        while not foundEmpty && not foundMyColour && squaresToTry > 0 do
            if Bitboard.isSet tryPos mySquares then
                foundMyColour <- true
            elif Bitboard.isSet tryPos oppSquares then
                flips <- Bitboard.set tryPos flips
                tryPos <- tryPos + direction
                squaresToTry <- squaresToTry - 1
            else
                foundEmpty <- true

        if foundMyColour then flips else Bitboard.empty

    let private anyFlips (board:Board) colour pos (direction, maxSquares) =
        let mutable squaresToTry = maxSquares

        let mutable tryPos = pos + direction
        let mutable foundEmpty = false
        let mutable foundColour = false
        let mutable foundFlip = false

        let mySquares, oppSquares =
            match colour with
            | Black -> board.BlackSquares, board.WhiteSquares
            | White -> board.WhiteSquares, board.BlackSquares

        while not foundEmpty && not foundColour && squaresToTry > 0 do
            if Bitboard.isSet tryPos mySquares then
                foundColour <- true
            elif Bitboard.isSet tryPos oppSquares then
                foundFlip <- true
                tryPos <- tryPos + direction
                squaresToTry <- squaresToTry - 1
            else
                foundEmpty <- true

        foundColour && foundFlip

    let private getFlips board colour pos =
        if not (Bitboard.isSet pos board.WhiteSquares || Bitboard.isSet pos board.BlackSquares) then
            let flips = preCalculatedDirectionsAndMaxSquares.[pos] |> Array.sumBy (fun directionAndMaxSquares -> findFlips board colour pos directionAndMaxSquares)
            flips
        else
            Bitboard.empty

    let private isEmptySquareAPossibleMove board colour pos =
        preCalculatedDirectionsAndMaxSquares.[pos] |> Array.exists (fun directionAndMaxSquares -> anyFlips board colour pos directionAndMaxSquares)

    let private moveResult board pos (flips:Bitboard) =
        let mutable whiteSquares = board.WhiteSquares ^^^ flips
        let mutable blackSquares = board.BlackSquares ^^^ flips

        match board.NextToMove with
        | White ->
            whiteSquares <- Bitboard.set pos whiteSquares
            blackSquares <- Bitboard.unset pos blackSquares
        | Black ->
            blackSquares <- Bitboard.set pos blackSquares
            whiteSquares <- Bitboard.unset pos whiteSquares

        { WhiteSquares = whiteSquares
          BlackSquares = blackSquares
          NextToMove = board.NextToMove.Opposite }

    let getPossibleMoves board = [|
        let allPieces = board.WhiteSquares ||| board.BlackSquares
        for pos in 0..63 do
            if not (Bitboard.isSet pos allPieces) && isEmptySquareAPossibleMove board board.NextToMove pos then
                yield pos
    |]

    let getPossibleMovesAndFlips board = [|
        for pos in 0..63 do
            let flips = getFlips board board.NextToMove pos
            if flips <> Bitboard.empty then
                yield { Pos = pos; Flips = flips; Result = moveResult board pos flips }
     |]

    let applyMove pos board =
        let flips = getFlips board board.NextToMove pos
        if flips <> Bitboard.empty then
            { Pos = pos; Flips = flips; Result = moveResult board pos flips }
        else
            failwithf "move is invalid"

    let anyPossibleMovesByOpposite board =
        let opposite = board.NextToMove.Opposite
        let allPieces = board.WhiteSquares ||| board.BlackSquares
        let mutable found = false
        let mutable pos = 0

        while not found && pos <= 63 do
            found <- not (Bitboard.isSet pos allPieces) && isEmptySquareAPossibleMove board opposite pos
            pos <- pos + 1

        found

    let getStatus board =
        let numBlack, numWhite = countPieces board
        if numBlack > numWhite then
            Win Black
        elif numWhite > numBlack then
            Win White
        else
            Tie

    let toGameInfo board =
        let possibleMoves = getPossibleMovesAndFlips board

        let state =
            if Array.isEmpty possibleMoves then
                if anyPossibleMovesByOpposite board then
                    OngoingSkipMove board
                else
                    Finished { Board = board; Result = getStatus board }
            else
                Ongoing { Board = board; PossibleMoves = possibleMoves }

        {
            State = state
        }
