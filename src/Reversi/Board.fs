namespace FableReversi.Reversi

[<Struct>]
type Colour =
    | Black
    | White
    member this.opposite =
        match this with
        | Black -> White
        | White -> Black

[<Struct>]
type Square =
    | Piece of Colour
    | Empty

type GameResult =
    | Win of Colour
    | Tie


type Board =
    {
        Squares: Square[]
        NextToMove: Colour
    }

    with

        override this.ToString() =
            let squareAt x y = this.Squares.[x + y * 8]
            let mutable s = "   "

            for y in 0..7 do
                s <- s + " " + string y
            s <- s + "\n"

            for x in 0..7 do
                s <- s + " " + string x + " "
                for y in 0..7 do
                    match squareAt x y with
                    | Empty ->
                        s <- s + "  "
                    | Piece Black ->
                        s <- s + " B"
                    | Piece White ->
                        s <- s + " W"
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
            let squareAt x y = this.Result.Squares.[x + y * 8]
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
                        match squareAt x y with
                        | Empty ->
                            s <- s + "  "
                        | Piece Black ->
                            s <- s + " B"
                        | Piece White ->
                            s <- s + " W"
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


    let countPieces squares =
        let mutable black = 0
        let mutable white = 0

        squares |> Array.iter (fun sq ->
            if sq = Piece Black then black <- black + 1
            if sq = Piece White then white <- white + 1)

        black, white

    let create squares nextToMove = {
        Squares = squares
        NextToMove = nextToMove
    }

    let startingBoard =
        let squares = Array.create 64 Empty
        squares.[27] <- Piece Black
        squares.[28] <- Piece White
        squares.[35] <- Piece White
        squares.[36] <- Piece Black

        create squares Black

    let inline squareAt board x y = board.Squares.[x + y * 8]

    let inline private isOnBoard x y  =
        x >= 0 && x < 8 && y >= 0 && y < 8

    let private wouldFlip' board colour x y dx dy =
        let mutable lx = x + dx
        let mutable ly = y + dy
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [| while isOnBoard lx ly && not foundEmpty && not foundColour do
                match squareAt board lx ly with
                | Empty -> foundEmpty <- true
                | Piece c when c = colour -> foundColour <- true
                | _ -> yield lx,ly
                lx <- lx + dx
                ly <- ly + dy
            |]

        if foundColour then flips else [||]


    let private wouldFlip board colour x y dx dy =
        let mutable lx = x + dx
        let mutable ly = y + dy
        let mutable foundEmpty = false
        let mutable foundColour = false
        let mutable foundFlip = false

        while isOnBoard lx ly && not foundEmpty && not foundColour do
            match squareAt board lx ly with
            | Empty -> foundEmpty <- true
            | Piece c when c = colour -> foundColour <- true
            | _ -> foundFlip <- true

            lx <- lx + dx
            ly <- ly + dy

        foundColour && foundFlip

    let isPossibleMove' board colour x y =
        if squareAt board x y = Empty then
            let flips = directions |> Array.collect (fun (dx,dy) -> wouldFlip' board colour x y dx dy)
            if Array.isEmpty flips then None else Some flips
        else
            None

    let isPossibleMove board colour x y =
        if squareAt board x y = Empty then
            directions |> Array.exists (fun (dx,dy) -> wouldFlip board colour x y dx dy)
        else
            false

    let private moveResult board x y flips =
        let newSquares = Array.copy board.Squares
        let opposite = board.NextToMove.opposite

        flips |> Array.iter (fun (fx,fy) ->
            match squareAt board fx fy with
            | Empty -> failwith "Tried to flip empty square"
            | Piece c when c = opposite -> newSquares.[fx + fy * 8] <- Piece board.NextToMove
            | _ -> failwith "Tried to flip same colour")

        newSquares.[x + y * 8] <- Piece board.NextToMove

        create newSquares opposite

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
        let opposite = board.NextToMove.opposite
        let mutable found = false
        for x in 0..7 do
            for y in 0..7 do
                found <- found || isPossibleMove board opposite x y

        found

    let getStatus board =
        let numBlack, numWhite = countPieces board.Squares
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
