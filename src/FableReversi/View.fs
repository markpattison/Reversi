
module FableReversi.View
open Browser.Types
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Types
open FableReversi.Reversi
open FableReversi.Reversi.Computer
open FableReversi.Reversi.Computer.Players
open FableReversi.Reversi.Runner

let computerPlayers =
    [
        Random
        Greedy
        FewestReplies
        Minimax (Heuristics.Basic, 0)
        Minimax (Heuristics.Basic, 1)
        Minimax (Heuristics.Basic, 2)
        MCTS 25
        MCTS 50
    ]

let toPieceIcon colour =
    [ Fa.i
        [ Fa.Size Fa.Fa2x
          Fa.Solid.Circle
          Fa.Props
            [ Style [ Color colour ] ] ]
        [] ]

let toCellProps colour : IHTMLProp list =
    [ Style
        [ TextAlign TextAlignOptions.Center
          VerticalAlign "middle"
          Height "50px"
          Width "50px"
          BackgroundColor colour ] ]

let blackPiece = toPieceIcon "#000000"
let whitePiece = toPieceIcon "#ffffff"
let blackFlipPiece = toPieceIcon "#606060"
let whiteFlipPiece = toPieceIcon "#d0d0d0"

let plainCellProps = toCellProps "#00b000"
let possibleMoveCellProps = toCellProps "#00d000"
let possibleMoveHoverCellProps = toCellProps "#00f000"
let wouldFlipCellProps = toCellProps "#00d000"

let button txt onClick =
    Button.button
        [ Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

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

let showSquare dispatch humanPlaying (location, square, view) =
    let cellProps : IHTMLProp list =
        match view with
        | Plain -> plainCellProps
        | PossibleMove -> possibleMoveCellProps
        | PossibleMoveHover -> possibleMoveHoverCellProps
        | WouldFlip -> wouldFlipCellProps

    let cellContent =
        match square, view with
        | Empty, _ -> []
        | Piece Black, WouldFlip -> blackFlipPiece
        | Piece Black, _ -> blackPiece
        | Piece White, WouldFlip -> whiteFlipPiece
        | Piece White, _ -> whitePiece

    let onHover = OnMouseOver (fun _ -> Hover location |> dispatch) :> IHTMLProp
    let onClick = OnClick (fun _ -> Click location |> dispatch) :> IHTMLProp

    let props = if humanPlaying then (onClick :: onHover :: cellProps) else cellProps

    td props cellContent

let showBoard dispatch humanPlaying boardView =
    let rows =
        boardView.SquareViews
        |> List.map (fun row ->
            tr [] (row |> List.map (showSquare dispatch humanPlaying)))

    Table.table
        [ Table.IsBordered
          Table.IsNarrow
          Table.Props
            [ Style [ TableLayout "fixed"; Height "400px"; Width "400px" ] ] ]
        [ tbody [] rows ]

let playerName player =
    match player with
    | Human -> "Human"
    | Computer c -> sprintf "Computer %s" (c.ToString())

let summary result =
    match result with
    | Win Black -> "Black wins!"
    | Win White -> "White wins!"
    | Tie -> "Game tied!"

let gameContent model dispatch =
    let gameInfo = model.GameInfo

    let blackToPlay, whiteToPlay, result =
        match gameInfo.Board.NextToMove, gameInfo.State with
        | _, Finished fg -> false, false, Some fg.Result
        | Black, _ -> true, false, None
        | White, _ -> false, true, None

    let humanPlaying = match model.CurrentPlayer with | Human -> true | _ -> false

    let showSkipButton =
        match humanPlaying, gameInfo.State with
        | true, OngoingSkipMove _ -> true
        | _ -> false

    Columns.columns []
      [ Column.column [ Column.Width (Screen.All, Column.Is8) ]
          [ showBoard dispatch humanPlaying model.BoardView ]
        Column.column []
            [ p []
                    [ sprintf "Black (%s): %i " (fst model.PlayerBlack) (gameInfo.Board.NumBlack) |> str
                      if blackToPlay then Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              p []
                    [ sprintf "White (%s): %i " (fst model.PlayerWhite) (gameInfo.Board.NumWhite) |> str
                      if whiteToPlay then Fa.i [ Fa.Solid.ArrowAltCircleLeft ] [] ]
              if showSkipButton then
                  br []
                  button "Skip move" (fun _ -> dispatch (GameAction SkipMove))
              match result with
              | None -> ()
              | Some r ->
                  br []
                  p [] [ r |> summary |> str ]
                  br []
                  p [] [ button "Restart game" (fun _ -> dispatch Restart) ]
                  br[]
                  p [] [ button "Change players" (fun _ -> dispatch ChangePlayers) ] ] ]

let content model dispatch =
    match model.OuterState with
    | Lobby lobbyOptions -> lobbyContent lobbyOptions (LobbyMsg >> dispatch)
    | Playing gameModel -> gameContent gameModel (GameMsg >> dispatch)

let view (model : Model) (dispatch : Msg -> unit) =

    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Fable Reversi" ] ] ]

          Section.section []
                [ Container.container []
                    [ content model dispatch ] ] ]
