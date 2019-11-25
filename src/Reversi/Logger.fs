namespace FableReversi.Reversi

type Logger =
    {
        mutable OutputRev: (int * string) list
    }
    member this.Log level s = this.OutputRev <- (level, s) :: this.OutputRev

    member this.Read maxLevel = this.OutputRev |> List.filter (fun (level, _) -> level <= maxLevel) |> List.map snd |> List.rev
    
    static member Create() = { OutputRev = [] }
