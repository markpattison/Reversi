module FableReversi.Lobby.View

open Browser.Types
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Shared.View
open FableReversi.Reversi.Computer
open FableReversi.Reversi.Computer.Players

let computerPlayers =
    [
        Random
        Greedy
        FewestReplies
        Minimax (Heuristics.Basic, 0)
        Minimax (Heuristics.Basic, 1)
        Minimax (Heuristics.Basic, 2)
        MCTS 25
        MCTS 200
    ]

let players =
    let computers = List.map (fun cp -> (ComputerChoice cp, cp.Name)) computerPlayers
    (HumanChoice, "Human") :: computers

let dropdown value key dispatch =
    let onChange (ev: Event) =
        match List.tryFind (fun (_, name) -> name = ev.Value) players with
        | Some (pc, _) -> dispatch pc
        | None -> ()

    let dropDownItems =
        players
        |> List.map (fun (_, name) -> option [ Value name ] [ str name ])

    let currentValue =
        match List.tryFind (fun (cp, _) -> cp = value) players with
        | Some (_, name) -> name
        | None -> ""

    Field.div []
        [ Control.div []
            [ Select.select
                [ Select.Props [ OnChange onChange; Key key ] ]
                [ select [ DefaultValue currentValue ] dropDownItems ] ] ]

let lobbyContent lobbyOptions dispatch =
    div []
        [ p []
            [ str "Black"
              dropdown lobbyOptions.PlayerBlackChoice "BlackDropdown" (ChangeBlackPlayer >> dispatch) ]
          br []
          p []
            [ str "White"
              dropdown lobbyOptions.PlayerWhiteChoice "WhiteDropdown" (ChangeWhitePlayer >> dispatch) ]
          br []
          button "Start game" (fun _ -> dispatch Start)]
