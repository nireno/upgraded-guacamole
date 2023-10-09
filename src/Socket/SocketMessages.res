open AppPrelude
type ioPlayerId = str_json
type ioTeamId = str_json
type ioCard = str_json
type ioInviteCode = string
type ioUsername = string
type ioClientState = str_json
type ioToast = str_json
type ioClientSettings = str_json

type clientToServer =
  | IO_JoinGame(ioUsername, ioClientSettings)
  | IO_StartPrivateGame(ioUsername, ioClientSettings)
  | IO_JoinPrivateGame(ioInviteCode, ioUsername, ioClientSettings)
  | IO_PlayCard(ioPlayerId, ioCard)
  | IO_Beg
  | IO_Stand
  | IO_GiveOne
  | IO_Deal
  | IO_RunPack
  | IO_FlipFinalTrump
  | IO_DealAgain
  | IO_LeaveGame
  | IO_PlayAgain(ioUsername, ioClientSettings)
  | IO_Rematch
  | IO_Substitute(ioUsername, ioClientSettings)
  | IO_PrivateToPublic
  | IO_RotateGuests
  | IO_TransitionGameNow
  | IO_Signal(PlayerSignal.t)

let stringOfClientToServer = x =>
  switch x {
  | IO_JoinGame(ioUsername, _) =>
    "JoinGame(" ++ ((ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")")
  | IO_StartPrivateGame(ioUsername, _) =>
    "StartPrivateGame(" ++ ((ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")")
  | IO_JoinPrivateGame(inviteCode, username, _) =>
    "JoinPrivateGame(" ++
    (inviteCode ++
    (", " ++ ((username == "" ? "--blank-username--" : username) ++ ")")))
  | IO_PlayCard(ioPlayerId, ioCard) => j`PlayCard($ioPlayerId, $ioCard)`
  | IO_Beg => "Beg"
  | IO_Stand => "Stand"
  | IO_GiveOne => "GiveOne"
  | IO_Deal => "Deal"
  | IO_RunPack => "RunPack"
  | IO_FlipFinalTrump => "FlipFinalTrump"
  | IO_DealAgain => "DealAgain"
  | IO_LeaveGame => "LeaveGame"
  | IO_PlayAgain(ioUsername, _ioClientSettings) => j`PlayAgain($ioUsername)`
  | IO_Rematch => "Rematch"
  | IO_Substitute(ioUsername, _ioClientSettings) => j`Substitute($ioUsername)`
  | IO_PrivateToPublic => "PrivateToPublic"
  | IO_RotateGuests => "RotateGuests"
  | IO_TransitionGameNow => "SelectPartner"
  | IO_Signal(_) => "Signal"
  }

type serverToClient =
  | SetState(ioClientState)
  | ShowToast(ioToast)
  | Reset
  | AckOk
  | AckError(string)
  | HandshakeFailed
  | ShowSignal(Quad.id, PlayerSignal.t)

let stringOfServerToClient = x => {
  switch x {
  | SetState(_) => "SetState"
  | ShowToast(_) => "ShowToast"
  | Reset => "Reset"
  | AckOk => "AckOk"
  | AckError(_) => "AckError"
  | HandshakeFailed => "HandshakeFailed"
  | ShowSignal(_, _) => "ShowSignal"
  }
}

type ack = serverToClient => unit
