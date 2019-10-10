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

type Board =
    {
        Squares: Square[]
        NextToMove: Colour
    }

type PossibleMove =
    {
        MoveLocation: Location
        Flips: Location list
        Result: Board
    }

type OngoingGame =
    {
        Board: Board
        PossibleMoves: PossibleMove list
    }

type GameState =
    | Ongoing of OngoingGame
    | OngoingSkipMove of Board
    | Finished of Board

type GameInfo =
    {
        State: GameState
        NumBlack: int
        NumWhite: int
    }
    member this.Board =
        match this.State with
        | Ongoing og -> og.Board
        | OngoingSkipMove b -> b
        | Finished b -> b
    member this.NextToMove =
        match this.State with
        | Ongoing og -> og.Board.NextToMove
        | OngoingSkipMove b -> b.NextToMove
        | Finished b -> b.NextToMove

module Board =

    let directions =
        [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
        |> List.map Location
    
    let startingBoard =
        let squares = Array.create 64 Empty
        squares.[27] <- Piece Black
        squares.[28] <- Piece White
        squares.[35] <- Piece White
        squares.[36] <- Piece Black

        { Squares = squares; NextToMove = Black }

    let private countPieces board =
        let mutable black = 0
        let mutable white = 0

        board.Squares |> Array.iter (fun sq ->
            if sq = Piece Black then black <- black + 1
            if sq = Piece White then white <- white + 1)
        
        (black, white)
    
    let private indexOf (Location (x, y)) =
        x + y * 8
    
    let squareAt board location = board.Squares.[indexOf location]

    let private isOnBoard (Location (x, y)) =
        x >= 0 && x < 8 && y >= 0 && y < 8
    
    let private wouldFlip board colour location direction =
        let mutable location' = location + direction
        let mutable foundEmpty = false
        let mutable foundColour = false

        let flips =
            [ while (isOnBoard location' && not foundEmpty && not foundColour) do
                match squareAt board location' with
                | Empty -> foundEmpty <- true
                | Piece c when c = colour -> foundColour <- true
                | _ -> yield location'
                location' <- location' + direction
            ]
        
        if foundColour then flips else []

    let private isPossibleMove board colour location =
        if squareAt board location = Empty then
            let flips = directions |> List.collect (wouldFlip board colour location)
            if flips.IsEmpty then None else Some flips
        else
            None
   
    let private moveResult board moveLocation flips =
        let newSquares = Array.copy board.Squares
        let opposite = board.NextToMove.opposite

        flips |> List.iter (fun flip ->
            match squareAt board flip with
            | Empty -> failwith "Tried to flip empty square"
            | Piece c when c = opposite -> newSquares.[indexOf flip] <- Piece board.NextToMove
            | _ -> failwith "Tried to flip same colour")

        newSquares.[indexOf moveLocation] <- Piece board.NextToMove

        { Squares = newSquares; NextToMove = opposite }
   

    let private getPossibleMoves board =
        [
            for x in 0..7 do
                for y in 0..7 do
                    let moveLocation = Location (x, y)
                    match isPossibleMove board board.NextToMove moveLocation with
                    | Some flips ->
                        yield { MoveLocation = moveLocation; Flips = flips; Result = moveResult board moveLocation flips }
                    | None -> ()
        ]
     
    let private anyPossibleMovesByOpposite board =
        let opposite = board.NextToMove.opposite

        let movesByOpposite =
            [
                for x in 0..7 do
                    for y in 0..7 do
                        let moveLocation = Location (x, y)
                        match isPossibleMove board opposite moveLocation with
                        | Some flips -> yield flips
                        | None -> ()
            ]
        
        not (List.isEmpty movesByOpposite)
    
    let toGameInfo board =
        let (blackCount, whiteCount) = countPieces board
        let possibleMoves = getPossibleMoves board

        let state =
            match possibleMoves with
            | [] -> if anyPossibleMovesByOpposite board then OngoingSkipMove board else Finished board
            | _ -> Ongoing { Board = board; PossibleMoves = possibleMoves }
        
        {
            State = state
            NumBlack = blackCount
            NumWhite = whiteCount
        }
