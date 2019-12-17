module FableReversi.TestComputer.Program

open Expecto

[<EntryPoint>]
let main args =
    testList "All tests" [
        TestMinimax.allTests
        TestMCTS.allTests
    ]
    |> runTestsWithArgs defaultConfig args
