module FableReversi.TestComputer.Tournament

open FableReversi.TestComputer.Game

let tournamentSummary results =
    let allResults =
        results
        |> Array.sortByDescending (fun pr -> pr.Wins * 10000 + pr.Ties * 100 + pr.PiecesFor - pr.PiecesAgainst)
        |> Array.map (fun pr -> sprintf "%s" (playerResultSummary pr))
        |> String.concat "\n"
    
    sprintf "\n%s\n" allResults

let playTournament players gamesPerSidePerPair =
    let numPlayers = Set.count players
    if numPlayers < 2 then failwith "Need at least two players for a tournament"

    let pairs =
        [|
            for _ in 1 .. gamesPerSidePerPair do
                for black in players do
                    for white in players do
                        if black <> white then yield (black, white)
        |]
    
    let results = Array.Parallel.map (fun (black, white) -> playGame black white) pairs

    let playerResults =
        results
        |> Array.collect (fun result -> result.PlayerResults)
        |> Array.groupBy (fun pr -> pr.Player)
        |> Array.map (snd >> Array.reduce (+))

    printfn "%s" (tournamentSummary playerResults)
