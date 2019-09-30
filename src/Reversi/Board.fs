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

type Board =
    {
        NextToMove: Colour
        Size: int
        Squares: Square []
    }

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
    
    let createEmpty boardSize =
        {
            NextToMove = Black
            Size = boardSize
            Squares = Array.create (boardSize * boardSize) Empty
        }

    let inline indexOf size (Location (x, y)) = x + y * size

    let private setPiece size (squares: Square []) location colour =
        let i = indexOf size location

        if squares.[i] = Empty then
            squares.[i] <- Piece colour
        else
            failwith "Tried to place a piece on an occupied square"

    let private getPiece board location =
        board.Squares.[indexOf board.Size location]

    let private flipPiece size (squares: Square []) location =
        let i = indexOf size location

        match squares.[i] with
        | Empty -> failwith "Tried to flip empty square"
        | Piece colour -> squares.[i] <- Piece (oppositeOf colour)

    let createStarting() =
        let board = createEmpty 8

        setPiece board.Size board.Squares (Location (3, 3)) Black
        setPiece board.Size board.Squares (Location (4, 4)) Black
        setPiece board.Size board.Squares (Location (3, 4)) White
        setPiece board.Size board.Squares (Location (4, 3)) White

        board

    let numPieces board colour =
        let mutable count = 0

        for i in 0..((board.Size - 1) * (board.Size - 1)) do
            if board.Squares.[i] = Piece colour then count <- count + 1
        
        count

    let pieceAt board location =
        board.Squares.[indexOf board.Size location]

    let private isOnBoard size (Location (x, y)) =
        x >= 0 && x < size && y >= 0 && y < size

    let private wouldFlip board location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [ while (isOnBoard board.Size location' && not foundEmpty && not foundColour) do
                match getPiece board location' with
                | Empty -> foundEmpty <- true
                | Piece c when c = board.NextToMove -> foundColour <- true
                | _ -> yield location'
                location' <- location' + direction
            ]
        
        if foundColour then flips else []

    let private directions =
        [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
        |> List.map Location

    let private isPossibleMove board location =
        if getPiece board location = Empty then
            let flips = directions |> List.collect (wouldFlip board location)
            if flips.IsEmpty then None else Some flips
        else
            None

    let private moveResult board moveLocation flips =
        let squares = Array.copy board.Squares

        flips |> List.iter (flipPiece board.Size squares)

        setPiece board.Size squares moveLocation board.NextToMove

        { board with Squares = squares; NextToMove = oppositeOf board.NextToMove }

    let possibleMoves board =
        [
            for x in 0..(board.Size - 1) do
                for y in 0..(board.Size - 1) do
                    let moveLocation = Location (x, y)
                    match isPossibleMove board moveLocation with
                    | Some flips ->
                        yield { MoveLocation = moveLocation; Flips = flips; Result = moveResult board moveLocation flips }
                    | None -> ()
        ]
