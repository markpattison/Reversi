module FableReversi.Reversi.Computer.Random

open FableReversi.Reversi
open FableReversi.Reversi.Computer.Runner

let player =
    let random = new System.Random()
    {
        ChooseMove = fun board ->
            let possibleMoves = board.PossibleMoves()
            let choice = random.Next(0, possibleMoves.Length)
            printfn "%i/%i" choice possibleMoves.Length
            printfn "%i %i" (board.NumPieces Black) (board.NumPieces White)
            possibleMoves |> List.iter (fun { MoveLocation = Location (x,y) } -> printfn "%i,%i" x y)
            possibleMoves.Item choice
    }
