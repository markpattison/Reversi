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

type Board private (size: int, nextToMove: Colour, squares: Square []) =

    static let directions =
        [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
        |> List.map Location
    
    public new() =
        let squares = Array.create 64 Empty
        squares.[27] <- Piece Black
        squares.[28] <- Piece White
        squares.[35] <- Piece White
        squares.[36] <- Piece Black
        Board(8, Black, squares)
    
    member private __.indexOf (Location (x, y)) = x + y * size

    member __.Size = size
    member __.NextToMove = nextToMove
    member this.Square location = squares.[this.indexOf location]
    member __.CopySquares() = Array.copy squares

    member __.NumPieces colour =
        let mutable count = 0

        squares |> Array.iter (fun sq ->
            if sq = Piece colour then count <- count + 1)
        
        count
    
    member private __.isOnBoard (Location (x, y)) =
        x >= 0 && x < size && y >= 0 && y < size
    
    member private this.wouldFlip location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [ while (this.isOnBoard location' && not foundEmpty && not foundColour) do
                match this.Square location' with
                | Empty -> foundEmpty <- true
                | Piece c when c = nextToMove -> foundColour <- true
                | _ -> yield location'
                location' <- location' + direction
            ]
        
        if foundColour then flips else []

    member private this.isPossibleMove location =
        if this.Square location = Empty then
            let flips = directions |> List.collect (this.wouldFlip location)
            if flips.IsEmpty then None else Some flips
        else
            None
   
    member private this.moveResult moveLocation flips =
        let newSquares = Array.copy squares
        let opposite = nextToMove.opposite

        flips |> List.iter (fun flip ->
            match this.Square flip with
            | Empty -> failwith "Tried to flip empty square"
            | Piece colour when colour = opposite -> newSquares.[this.indexOf flip] <- Piece nextToMove
            | _ -> failwith "Tried to flip same colour")

        newSquares.[this.indexOf moveLocation] <- Piece nextToMove

        Board(size, opposite, newSquares)
    
    member this.PossibleMoves() =
        [
            for x in 0..(size - 1) do
                for y in 0..(size - 1) do
                    let moveLocation = Location (x, y)
                    match this.isPossibleMove moveLocation with
                    | Some flips ->
                        yield { MoveLocation = moveLocation; Flips = flips; Result = this.moveResult moveLocation flips }
                    | None -> ()
        ]

and PossibleMove =
    {
        MoveLocation: Location
        Flips: Location list
        Result: Board
    }
