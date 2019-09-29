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

type PossibleMove =
    {
        MoveLocation: Location
        Flips: Location list
    }

type Position =
    {
        NextToMove: Colour
        Size: int
        Squares: Square []
    }

module Position =
    let createEmpty boardSize =
        {
            NextToMove = Black
            Size = boardSize
            Squares = Array.create (boardSize * boardSize) Empty
        }

    let inline indexOf size (Location (x, y)) = x + y * size

    let private setPiece position location square =
        position.Squares.[indexOf position.Size location] <- square

    let private getPiece position location =
        position.Squares.[indexOf position.Size location]

    let createStarting() =
        let position = createEmpty 8

        Piece Black |> setPiece position (Location (3, 3))
        Piece Black |> setPiece position (Location (4, 4))
        Piece White |> setPiece position (Location (3, 4))
        Piece White |> setPiece position (Location (4, 3))

        position

    let numPieces position colour =
        let mutable count = 0

        for i in 0..((position.Size - 1) * (position.Size - 1)) do
            if position.Squares.[i] = Piece colour then count <- count + 1
        
        count

    let pieceAt position location =
        position.Squares.[indexOf position.Size location]

    let opposite colour =
        match colour with
        | White -> Black
        | Black -> White

    let private isOnBoard size (Location (x, y)) =
        x >= 0 && x < size && y >= 0 && y < size

    let private wouldFlip position location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        [ while (isOnBoard position.Size location' && not foundEmpty && not foundColour) do
            match getPiece position location' with
            | Empty -> foundEmpty <- true
            | Piece c when c = position.NextToMove -> foundColour <- true
            | _ -> yield location'
            location' <- location' + direction
        ]

    let private directions =
        [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
        |> List.map Location

    let private isPossibleMove position location =
        if getPiece position location = Empty then
            let flips = directions |> List.collect (wouldFlip position location)
            if flips.IsEmpty then None else Some flips
        else
            None

    let possibleMoves position =
        [
            for x in 0..(position.Size - 1) do
                for y in 0..(position.Size - 1) do
                    match isPossibleMove position (Location (x, y)) with
                    | Some flips -> yield { MoveLocation = (Location (x, y)); Flips = flips }
                    | None -> ()
        ]
