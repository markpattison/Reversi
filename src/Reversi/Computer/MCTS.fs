module FableReversi.Reversi.Computer.MCTS

open FableReversi.Reversi
open FableReversi.Reversi.Runner
open System

let c = System.Math.Sqrt(2.)

[<RequireQualifiedAccess>]
type Children =
| Unknown
| Discarded
| SkipMove of Node
| Moves of Node []

and Node(parent:Node option,random:Random,board:Board) =
    let mutable wins = 0.
    let mutable tries = 0.
    let mutable chances = 0.5
    let mutable children  = ref Children.Unknown

    with
        member __.Board = board
        member __.Chances = chances
        member __.Wins = wins
        member __.Tries = tries

        member this.ApplyMove move =
            match !children with
            | Children.Unknown ->
                this.Expand()
                this.ApplyMove move
            | Children.Discarded -> failwithf "Can't select move on discarded children"
            | Children.SkipMove node -> node
            | Children.Moves cs ->
                let node = cs |> Array.find (fun (n:Node) -> n.Board = move)
                children := Children.Discarded
                node

        member this.ApplyBestMove() =
            match !children with
            | Children.Unknown ->
                this.Expand()
                this.ApplyBestMove()
            | Children.Discarded -> failwithf "Can't select move on discarded children"
            | Children.SkipMove node -> node
            | Children.Moves cs ->
                let best = cs |> Array.maxBy (fun (n:Node) -> n.Chances)
                let allBestMoves = cs |> Array.filter (fun m -> m.Chances = best.Chances)
                let choice = random.Next(0, allBestMoves.Length)

                children := Children.Discarded
                allBestMoves.[choice]

        member __.BackProp amount =
            tries <- tries + 1.
            if amount = 0.5 then
                wins <- wins + 0.5
            else
                if board.NextToMove = White && amount = -1. then
                    wins <- wins + 1.
                if board.NextToMove = Black && amount = 1. then
                   wins <- wins + 1.

            chances <- wins / tries

            match parent with
            | Some (parent:Node) -> parent.BackProp amount
            | _ -> ()

        member this.Playout() =
            let mutable currentBoard = board
            let mutable isDone = false

            while not isDone do
                let moves = Board.getPossibleMoves currentBoard
                if Array.isEmpty moves then
                    if Board.anyPossibleMovesByOpposite currentBoard then
                        let opposite = currentBoard.NextToMove.Opposite
                        currentBoard <- { currentBoard with NextToMove = opposite }
                    else
                        let numBlack = Bitboard.count currentBoard.BlackSquares
                        let numWhite = Bitboard.count currentBoard.WhiteSquares
                        if numBlack > numWhite then
                            this.BackProp -1.
                        elif numWhite > numBlack then
                            this.BackProp 1.
                        else
                            this.BackProp 0.5

                        isDone <- true
                else
                    let choice = random.Next(0, moves.Length)
                    let m = moves.[choice]
                    let move = Board.applyMove m currentBoard
                    currentBoard <- move.Result

        member this.Select() =
            match !children with
            | Children.Unknown ->
                this
            | Children.Discarded ->
                failwithf "Can't select move on discarded children"
            | Children.SkipMove node ->
                node.Select()
            | Children.Moves children ->
                if Array.isEmpty children then
                    this
                else
                    let n = System.Math.Log(tries)
                    let f (node:Node) = node.Chances + c * System.Math.Sqrt(n / node.Tries)
                    (children |> Seq.maxBy f).Select()

        member this.Expand() =
            if !children = Children.Unknown then
                children :=
                    let moves = Board.getPossibleMovesAndFlips board
                    if Array.isEmpty moves then
                        let opposite = board.NextToMove.Opposite
                        Children.SkipMove(Node(Some this,random,{ board with NextToMove = opposite }))
                    else
                        moves
                        |> Array.map (fun move ->
                            let n = Node(Some this,random,move.Result)
                            n.Playout()
                            n)
                        |> Children.Moves

let create playouts =
    let random = Random()
    let mutable current = Node(None,random,Board.startingBoard)
    let mutable moveIndex = 0

    let logger = ignore //printfn "%s"

    {
        OnMoveSkipped = fun () -> current <- current.ApplyBestMove()
        OpponentSelected = fun selected -> current <- current.ApplyMove selected.Result
        ChooseMove = fun ongoingGame ->
            for _ in 1 .. playouts do
                let selected = current.Select()
                selected.Expand()
                selected.Playout()

            moveIndex <- moveIndex + 1
            current <- current.ApplyBestMove()
            sprintf "Move %i: chances: %.3f tries: %d" moveIndex current.Chances (int current.Tries) |> logger

            let selected = ongoingGame.PossibleMoves |> Array.find (fun m -> m.Result = current.Board)
            selected
    }
