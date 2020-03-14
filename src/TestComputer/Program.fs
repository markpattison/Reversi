module FableReversi.TestComputer.Program

open Expecto

[<EntryPoint>]
let main args =
    testList "All tests" [
        TestMinimax.allTests
        TestMCTS.allTests
        //TestTournament.allTests  // slow!
    ]
    |> runTestsWithArgs defaultConfig args
