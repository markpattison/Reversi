namespace FableReversi.Reversi

[<Struct>]
type Colour = Black | White

[<Struct>]
type Square =
    | Piece of Colour
    | Empty

[<Struct>]
type Location =
    | Location of int * int
    static member (+) ((Location (x1, y1)), (Location (x2, y2))) = Location (x1 + x2, y1 + y2)

type Board(size: int, nextToMove: Colour, squares: Square []) =

    do if squares.Length <> size * size then failwith "Incorrect number of squares"

    member private __.indexOf (Location (x, y)) = x + y * size

    member __.Size = size
    member __.NextToMove = nextToMove
    member this.Square(location) = squares.[this.indexOf location]
    member __.CopySquares() = Array.copy squares

    member __.NumPieces(colour) =
        let mutable count = 0

        squares |> Array.iter (fun sq ->
            if sq = Piece colour then count <- count + 1)
        
        count

type PossibleMove =
    {
        MoveLocation: Location
        Flips: Location list
        Result: Board
    }

module Board =
    let oppositeOf colour =
        match colour with
        | White -> Black
        | Black -> White
    
    let inline indexOf size (Location (x, y)) = x + y * size

    let private setPiece size (squares: Square []) location colour =
        let i = indexOf size location

        if squares.[i] = Empty then
            squares.[i] <- Piece colour
        else
            failwith "Tried to place a piece on an occupied square"

    let private flipPiece size (squares: Square []) location =
        let i = indexOf size location

        match squares.[i] with
        | Empty -> failwith "Tried to flip empty square"
        | Piece colour -> squares.[i] <- Piece (oppositeOf colour)

    let createStarting() =
        let squares = Array.create 64 Empty

        squares.[27] <- Piece Black
        squares.[28] <- Piece White
        squares.[35] <- Piece White
        squares.[36] <- Piece Black

        Board(8, Black, squares)

    let private isOnBoard size (Location (x, y)) =
        x >= 0 && x < size && y >= 0 && y < size

    let private wouldFlip (board: Board) location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [ while (isOnBoard board.Size location' && not foundEmpty && not foundColour) do
                match board.Square location' with
                | Empty -> foundEmpty <- true
                | Piece c when c = board.NextToMove -> foundColour <- true
                | _ -> yield location'
                location' <- location' + direction
            ]
        
        if foundColour then flips else []

    let private directions =
        [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
        |> List.map Location

    let private isPossibleMove (board: Board) location =
        if board.Square location = Empty then
            let flips = directions |> List.collect (wouldFlip board location)
            if flips.IsEmpty then None else Some flips
        else
            None

    let private moveResult (board: Board) moveLocation flips =
        let squares = board.CopySquares()

        flips |> List.iter (flipPiece board.Size squares)

        setPiece board.Size squares moveLocation board.NextToMove

        Board(board.Size, oppositeOf board.NextToMove, squares)

    let possibleMoves (board: Board) =
        [
            for x in 0..(board.Size - 1) do
                for y in 0..(board.Size - 1) do
                    let moveLocation = Location (x, y)
                    match isPossibleMove board moveLocation with
                    | Some flips ->
                        yield { MoveLocation = moveLocation; Flips = flips; Result = moveResult board moveLocation flips }
                    | None -> ()
        ]
