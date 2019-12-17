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

[<Struct>]
type Location =
    | Location of int * int
    static member (+) ((Location (x1, y1)), (Location (x2, y2))) = Location (x1 + x2, y1 + y2)



type GameResult =
    | Win of Colour
    | Tie


type Board =
    {
        Squares: Square[]
        NextToMove: Colour
        NumBlack: int
        NumWhite: int
    }

    with
        member this.Status =
            if this.NumBlack > this.NumWhite then
                Win Black
            elif this.NumWhite > this.NumBlack then Win White else Tie

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
        MoveLocation: Location
        Flips: Location []
        Result: Board
    }

    with
        override this.ToString() =
            let lx, ly = match this.MoveLocation with Location(x,y) -> x,y
            let squareAt x y = this.Result.Squares.[x + y * 8]
            let mutable s = "   "

            for y in 0..7 do
                s <- s + " " + string y
            s <- s + "\n"

            for x in 0..7 do
                s <- s + " " + string x + " "
                for y in 0..7 do
                    if x = lx && y = ly then
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
        |> Array.map Location


    let private countPieces squares =
        let mutable black = 0
        let mutable white = 0

        squares |> Array.iter (fun sq ->
            if sq = Piece Black then black <- black + 1
            if sq = Piece White then white <- white + 1)

        (black, white)

    let create squares nextToMove =
        let numBlack, numWhite = countPieces squares
        {
            Squares = squares
            NextToMove = nextToMove
            NumBlack = numBlack
            NumWhite = numWhite
        }

    let startingBoard =
        let squares = Array.create 64 Empty
        squares.[27] <- Piece Black
        squares.[28] <- Piece White
        squares.[35] <- Piece White
        squares.[36] <- Piece Black

        create squares Black

    let private indexOf (Location (x, y)) =
        x + y * 8

    let squareAt board location = board.Squares.[indexOf location]

    let private isOnBoard (Location (x, y)) =
        x >= 0 && x < 8 && y >= 0 && y < 8

    let private wouldFlip' board colour location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [| while (isOnBoard location' && not foundEmpty && not foundColour) do
                match squareAt board location' with
                | Empty -> foundEmpty <- true
                | Piece c when c = colour -> foundColour <- true
                | _ -> yield location'
                location' <- location' + direction
            |]

        if foundColour then flips else [||]


    let private wouldFlip board colour location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false
        let mutable foundFlip = false

        while (isOnBoard location' && not foundEmpty && not foundColour) do
            match squareAt board location' with
            | Empty -> foundEmpty <- true
            | Piece c when c = colour -> foundColour <- true
            | _ -> foundFlip <- true
            location' <- location' + direction

        foundColour && foundFlip

    let isPossibleMove' board colour location =
        if squareAt board location = Empty then
            let flips = directions |> Array.collect (wouldFlip' board colour location)
            if Array.isEmpty flips then None else Some flips
        else
            None

    let isPossibleMove board colour location =
        if squareAt board location = Empty then
            directions |> Array.exists (wouldFlip board colour location)
        else
            false

    let private moveResult board moveLocation flips =
        let newSquares = Array.copy board.Squares
        let opposite = board.NextToMove.opposite

        flips |> Array.iter (fun flip ->
            match squareAt board flip with
            | Empty -> failwith "Tried to flip empty square"
            | Piece c when c = opposite -> newSquares.[indexOf flip] <- Piece board.NextToMove
            | _ -> failwith "Tried to flip same colour")

        newSquares.[indexOf moveLocation] <- Piece board.NextToMove

        create newSquares opposite

    let getPossibleMoves board =
        [|
            for x in 0..7 do
                for y in 0..7 do
                    let moveLocation = Location (x, y)
                    if isPossibleMove board board.NextToMove moveLocation then
                        yield moveLocation
        |]


    let getPossibleMoves' board =
        [|
            for x in 0..7 do
                for y in 0..7 do
                    let moveLocation = Location (x, y)
                    match isPossibleMove' board board.NextToMove moveLocation with
                    | Some flips ->
                        yield { MoveLocation = moveLocation; Flips = flips; Result = moveResult board moveLocation flips }
                    | None -> ()
        |]

    let applyMove moveLocation board =
        match isPossibleMove' board board.NextToMove moveLocation with
        | Some flips ->
            { MoveLocation = moveLocation; Flips = flips; Result = moveResult board moveLocation flips }
        | None ->
            failwithf "move is invalid"

    let anyPossibleMovesByOpposite board =
        let opposite = board.NextToMove.opposite
        let mutable found = false
        for x in 0..7 do
            for y in 0..7 do
                found <- found || isPossibleMove board opposite (Location (x, y))

        found

    let toGameInfo board =
        let possibleMoves = getPossibleMoves' board

        let state =
            if Array.isEmpty possibleMoves then
                if anyPossibleMovesByOpposite board then
                    OngoingSkipMove board
                else
                    Finished { Board = board; Result = board.Status }
            else
                Ongoing { Board = board; PossibleMoves = possibleMoves }

        {
            State = state
        }
