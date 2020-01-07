namespace FableReversi.Reversi

module Bitwise =
    let inline pos (x:int) (y:int) = x + 8 * y

    let inline setStone (pos:int) (board:uint64) =
        board ||| (1UL <<< pos)

    let inline removeStone (pos:int) (board:uint64) =
        board &&& (~~~ (1UL <<< pos))

    let inline isSet (pos:int) (board:uint64) =
        ((board >>> pos ) &&& 1UL) = 1UL

    let countStones (board:uint64) =
        let mutable count = 0UL
        let mutable board = board
        while board > 0UL do
            count <- count + (board &&& 1UL)
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
        X: int
        Y: int
        Flips: (int * int) []
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
                    if x = this.X && y = this.Y then
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

    let inline create blackSquares whiteSquares nextToMove = {
        WhiteSquares = whiteSquares
        BlackSquares = blackSquares
        NextToMove = nextToMove
    }

    let startingBoard =
        let blackSquares =
            0UL
            |> Bitwise.setStone (Bitwise.pos 3 3)
            |> Bitwise.setStone (Bitwise.pos 4 4)

        let whiteSquares =
            0UL
            |> Bitwise.setStone (Bitwise.pos 4 3)
            |> Bitwise.setStone (Bitwise.pos 3 4)

        create blackSquares whiteSquares Black

    let inline private isOnBoard x y  =
        x >= 0 && x < 8 && y >= 0 && y < 8

    let private wouldFlip' (board:Board) colour x y dx dy =
        let mutable lx = x + dx
        let mutable ly = y + dy
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [| while isOnBoard lx ly && not foundEmpty && not foundColour do
                let pos = Bitwise.pos lx ly
                if Bitwise.isSet pos board.WhiteSquares then
                    if colour = White then
                        foundColour <- true
                    else
                        yield lx,ly
                elif Bitwise.isSet pos board.BlackSquares then
                    if colour = Black then
                        foundColour <- true
                    else
                        yield lx,ly
                else
                    foundEmpty <- true

                lx <- lx + dx
                ly <- ly + dy
            |]

        if foundColour then flips else [||]


    let private wouldFlip (board:Board) colour x y dx dy =
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

    let isPossibleMove' board colour x y =
        let pos = Bitwise.pos x y
        if not (Bitwise.isSet pos board.WhiteSquares || Bitwise.isSet pos board.BlackSquares) then
            let flips = directions |> Array.collect (fun (dx,dy) -> wouldFlip' board colour x y dx dy)
            if Array.isEmpty flips then None else Some flips
        else
            None

    let isPossibleMove board colour x y =
        let pos = Bitwise.pos x y
        if not (Bitwise.isSet pos board.WhiteSquares || Bitwise.isSet pos board.BlackSquares) then
            directions |> Array.exists (fun (dx,dy) -> wouldFlip board colour x y dx dy)
        else
            false

    let private moveResult board x y flips =
        let opposite = board.NextToMove.Opposite
        let mutable whiteSquares = board.WhiteSquares
        let mutable blackSquares = board.BlackSquares

        flips
        |> Array.iter (fun (fx,fy) ->
            let pos = Bitwise.pos fx fy
            if Bitwise.isSet pos whiteSquares then
                whiteSquares <- Bitwise.removeStone pos whiteSquares
                blackSquares <- Bitwise.setStone pos blackSquares
            elif Bitwise.isSet pos blackSquares then
                whiteSquares <- Bitwise.setStone pos whiteSquares
                blackSquares <- Bitwise.removeStone pos blackSquares
            else
                failwith "Tried to flip empty square")

        let pos = Bitwise.pos x y
        match board.NextToMove with
        | White ->
            whiteSquares <- Bitwise.setStone pos whiteSquares
            blackSquares <- Bitwise.removeStone pos blackSquares
        | Black ->
            blackSquares <- Bitwise.setStone pos blackSquares
            whiteSquares <- Bitwise.removeStone pos whiteSquares

        create blackSquares whiteSquares opposite

    let getPossibleMoves board =
        [|
            for x in 0..7 do
                for y in 0..7 do
                    if isPossibleMove board board.NextToMove x y then
                        yield x,y
        |]


    let getPossibleMoves' board =
        [|
            for x in 0..7 do
                for y in 0..7 do
                    match isPossibleMove' board board.NextToMove x y with
                    | Some flips ->
                        yield { X = x; Y = y; Flips = flips; Result = moveResult board x y flips }
                    | None -> ()
        |]

    let applyMove x y board =
        match isPossibleMove' board board.NextToMove x y with
        | Some flips ->
            { X = x; Y = y; Flips = flips; Result = moveResult board x y flips }
        | None ->
            failwithf "move is invalid"

    let anyPossibleMovesByOpposite board =
        let opposite = board.NextToMove.Opposite
        let mutable found = false
        for x in 0..7 do
            for y in 0..7 do
                found <- found || isPossibleMove board opposite x y

        found

    let getStatus board =
        let numBlack = Bitwise.countStones board.BlackSquares
        let numWhite = Bitwise.countStones board.WhiteSquares
        if numBlack > numWhite then
            Win Black
        elif numWhite > numBlack then Win White else Tie

    let toGameInfo board =
        let possibleMoves = getPossibleMoves' board

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
