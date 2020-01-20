module FableReversi.TestComputer.Series

open FableReversi.TestComputer.Game

let seriesSummary pr1 pr2 =
    sprintf "\n%s\n%s\n" (playerResultSummary pr1) (playerResultSummary pr2)

let playSeries playerOne playerTwo gamesPerSide =
    let results =
        Array.init
            (2 * gamesPerSide)
            (fun i -> if i % 2 = 0 then playGame playerOne playerTwo else playGame playerTwo playerOne)

    let playerResults = results |> Array.collect (fun result -> result.PlayerResults)
    let playerOneResults = playerResults |> Array.filter (fun pr -> pr.Player = playerOne) |> Array.reduce (+)
    let playerTwoResults = playerResults |> Array.filter (fun pr -> pr.Player = playerTwo) |> Array.reduce (+)

    printfn "%s" (seriesSummary playerOneResults playerTwoResults)
