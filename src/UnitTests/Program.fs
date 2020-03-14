module FableReversi.TestComputer.Program

open Expecto

[<EntryPoint>]
let main args =
    testList "All tests" [
        TestBoard.allTests
    ]
    |> runTestsWithArgs defaultConfig args
