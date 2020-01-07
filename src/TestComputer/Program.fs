module FableReversi.TestComputer.Program

open Expecto

[<EntryPoint>]
let main args =
    testList "All tests" [
        TestBoard.allTests
        TestMinimax.allTests
        TestMCTS.allTests
    ]
    |> runTestsWithArgs defaultConfig args
