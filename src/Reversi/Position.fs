module FableReversi.Reversi.Position

[<Struct>]
type Colour = Black | White

[<Struct>]
type Square =
    | Piece of Colour
    | Empty

[<Struct>]
type Location = { X: int; Y: int }

type PossibleMove =
    {
        MoveLocation: Location
        Flips: Location list
    }

type Position =
    private {
        NextToMove: Colour
        Size: int
        Squares: Square [,]
    }

let createEmpty boardSize =
    {
        NextToMove = Black
        Size = boardSize
        Squares = Array2D.create boardSize boardSize Empty
    }

let createStarting() =
    let position = createEmpty 8

    position.Squares.[3, 3] <- Piece Black
    position.Squares.[4, 4] <- Piece Black
    position.Squares.[3, 4] <- Piece White
    position.Squares.[4, 3] <- Piece White

    position

let numPieces (squares: Square [,]) size colour =
    let mutable count = 0

    for x in 0..(size - 1) do
        for y in 0..(size - 1) do
            if squares.[x, y] = Piece colour then count <- count + 1
    
    count

let pieceAt position location =
    position.Squares.[location.X, location.Y]

let opposite colour =
    match colour with
    | White -> Black
    | Black -> White

let private wouldFlip position x y (xInc, yInc) =
    let mutable x' = x + xInc
    let mutable y' = y + yInc
    let mutable foundEmpty = false
    let mutable foundColour = false

    [ while (x' >= 0 && x' < position.Size && y' >= 0 && y' < position.Size && not foundEmpty && not foundColour) do
        match position.Squares.[x, y] with
        | Empty -> foundEmpty <- true
        | Piece c when c = position.NextToMove -> foundColour <- true
        | _ -> yield { X = x'; Y = y' }
        x' <- x' + xInc
        y' <- y' + yInc
    ]

let private directions = [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]

let private isPossibleMove position x y =
    if position.Squares.[x, y] = Empty then
        None
    else
        let flips = directions |> List.collect (wouldFlip position x y)
        if flips.IsEmpty then None else Some flips

let possibleMoves position =
    [
        for x in 0..(position.Size - 1) do
            for y in 0..(position.Size - 1) do
                match isPossibleMove position x y with
                | Some flips -> yield { MoveLocation = { X = x; Y = y }; Flips = flips }
                | None -> ()
    ]
