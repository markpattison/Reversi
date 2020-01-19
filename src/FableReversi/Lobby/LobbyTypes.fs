module FableReversi.Lobby.Types

open FableReversi.Reversi.Computer.Players

type PlayerChoice =
    | HumanChoice
    | ComputerChoice of ComputerPlayerChoice

type LobbyModel =
    { PlayerBlackChoice: PlayerChoice
      PlayerWhiteChoice: PlayerChoice }

type LobbyMsg =
    | ChangeBlackPlayer of PlayerChoice
    | ChangeWhitePlayer of PlayerChoice
    | Start
