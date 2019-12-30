namespace FableReversi.Reversi

// use level 0 for per-move
// level -1 for per-game
// levels 1, 2, ... for depths within a move

type Logger =
    {
        mutable OutputRev: (int * string) list
    }
    member this.Log level s = this.OutputRev <- (level, s) :: this.OutputRev

    member this.Read maxLevel = this.OutputRev |> List.filter (fun (level, _) -> level <= maxLevel) |> List.map snd |> List.rev

    member this.Print maxLevel = this.Read maxLevel |> List.iter (printfn "%s")
    
    static member Create() = { OutputRev = [] }
