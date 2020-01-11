namespace FableReversi.Reversi

module Bitwise =
    let inline pos (x:int) (y:int) = x + 8 * y

    let inline getXY (pos:int) = pos % 8, pos / 8

    let inline setStone (pos:int) (board:uint64) =
        board ||| (1UL <<< pos)

    let inline removeStone (pos:int) (board:uint64) =
        board &&& (~~~ (1UL <<< pos))

    let inline isSet (pos:int) (board:uint64) =
        ((board >>> pos ) &&& 1UL) = 1UL

    let countStones (board:uint64) =
        let mutable count = 0
        let mutable board = board
        while board > 0UL do
            count <- count + int (board &&& 1UL)
            board <- board >>> 1

        count

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
        WhiteSquares: uint64
        BlackSquares: uint64
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
                    let pos = Bitwise.pos x y
                    if Bitwise.isSet pos this.BlackSquares then
                        s <- s + " B"
                    elif Bitwise.isSet pos this.WhiteSquares then
                        s <- s + " W"
                    else
                        s <- s + "  "
                s <- s + "\n"
            s

type PossibleMove =
    {
        Pos: int
        Flips: uint64
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
                    let pos = Bitwise.pos x y
                    if this.Pos = pos then
                        s <- s + " *"
                    else
                        let pos = Bitwise.pos x y
                        if Bitwise.isSet pos this.Result.BlackSquares then
                            s <- s + " B"
                        elif Bitwise.isSet pos this.Result.WhiteSquares then
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

    let private directions =
        [| (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) |]

    let startingBoard = {
        WhiteSquares =
            0UL
            |> Bitwise.setStone (Bitwise.pos 4 3)
            |> Bitwise.setStone (Bitwise.pos 3 4)

        BlackSquares =
            0UL
            |> Bitwise.setStone (Bitwise.pos 3 3)
            |> Bitwise.setStone (Bitwise.pos 4 4)

        NextToMove = Black
    }

    let inline private isOnBoard x y =
        x >= 0 && x < 8 && y >= 0 && y < 8

    let private wouldFlip' (board:Board) colour pos dx dy =
        let x,y = Bitwise.getXY pos
        let mutable lx = x + dx
        let mutable ly = y + dy
        let mutable foundEmpty = false
        let mutable foundColour = false
        let mutable flips = 0UL

        while isOnBoard lx ly && not foundEmpty && not foundColour do
            let pos = Bitwise.pos lx ly
            if Bitwise.isSet pos board.WhiteSquares then
                if colour = White then
                    foundColour <- true
                else
                    flips <- Bitwise.setStone pos flips
            elif Bitwise.isSet pos board.BlackSquares then
                if colour = Black then
                    foundColour <- true
                else
                    flips <- Bitwise.setStone pos flips
            else
                foundEmpty <- true

            lx <- lx + dx
            ly <- ly + dy

        if foundColour then flips else 0UL

    let private wouldFlip (board:Board) colour pos dx dy =
        let x,y = Bitwise.getXY pos
        let mutable lx = x + dx
        let mutable ly = y + dy
        let mutable foundEmpty = false
        let mutable foundColour = false
        let mutable foundFlip = false

        while isOnBoard lx ly && not foundEmpty && not foundColour do
            let pos = Bitwise.pos lx ly
            if Bitwise.isSet pos board.WhiteSquares then
                if colour = White then
                    foundColour <- true
                else
                    foundFlip <- true
            elif Bitwise.isSet pos board.BlackSquares then
                if colour = Black then
                    foundColour <- true
                else
                    foundFlip <- true
            else
                foundEmpty <- true

            lx <- lx + dx
            ly <- ly + dy

        foundColour && foundFlip

    let getFlips board colour pos =
        if not (Bitwise.isSet pos board.WhiteSquares || Bitwise.isSet pos board.BlackSquares) then
            let flips = directions |> Array.sumBy (fun (dx,dy) -> wouldFlip' board colour pos dx dy)
            flips
        else
            0UL

    let isPossibleMove board colour pos =
        if not (Bitwise.isSet pos board.WhiteSquares || Bitwise.isSet pos board.BlackSquares) then
            directions |> Array.exists (fun (dx,dy) -> wouldFlip board colour pos dx dy)
        else
            false

    let private moveResult board pos (flips:uint64) =
        let mutable whiteSquares = board.WhiteSquares ^^^ flips
        let mutable blackSquares = board.BlackSquares ^^^ flips

        match board.NextToMove with
        | White ->
            whiteSquares <- Bitwise.setStone pos whiteSquares
            blackSquares <- Bitwise.removeStone pos blackSquares
        | Black ->
            blackSquares <- Bitwise.setStone pos blackSquares
            whiteSquares <- Bitwise.removeStone pos whiteSquares


        { WhiteSquares = whiteSquares
          BlackSquares = blackSquares
          NextToMove = board.NextToMove.Opposite }


    let getPossibleMoves board = [|
        for pos in 0..63 do
            if isPossibleMove board board.NextToMove pos then
                yield pos
    |]

    let getPossibleMovesAndFlips board = [|
        for pos in 0..63 do
            let flips = getFlips board board.NextToMove pos
            if flips <> 0UL then
                yield { Pos = pos; Flips = flips; Result = moveResult board pos flips }
     |]

    let applyMove pos board =
        let flips = getFlips board board.NextToMove pos
        if flips <> 0UL then
            { Pos = pos; Flips = flips; Result = moveResult board pos flips }
        else
            failwithf "move is invalid"

    let anyPossibleMovesByOpposite board =
        let opposite = board.NextToMove.Opposite
        let mutable found = false
        for pos in 0..63 do
            found <- found || isPossibleMove board opposite pos

        found

    let getStatus board =
        let numBlack = Bitwise.countStones board.BlackSquares
        let numWhite = Bitwise.countStones board.WhiteSquares
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
