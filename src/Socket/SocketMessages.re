open AppPrelude;
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
  | IO_EndTrick
  | IO_Beg
  | IO_Stand
  | IO_GiveOne
  | IO_Deal
  | IO_RunPack
  | IO_DealAgain
  | IO_LeaveGame
  | IO_PlayAgain(ioUsername, ioClientSettings)
  | IO_Rematch
  | IO_Substitute(ioUsername, ioClientSettings)
  | IO_PrivateToPublic
  | IO_SelectPartner
  | IO_StartGameNow;

let stringOfClientToServer = fun
  | IO_JoinGame(ioUsername, _) => "JoinGame(" ++ (ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")"
  | IO_StartPrivateGame(ioUsername, _) => "StartPrivateGame(" ++ (ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")"
  | IO_JoinPrivateGame(inviteCode, username, _) => "JoinPrivateGame(" ++ inviteCode ++ ", " ++ (username == "" ? "--blank-username--" : username) ++ ")"
  | IO_PlayCard(ioPlayerId, ioCard) => {j|PlayCard($ioPlayerId, $ioCard)|j}
  | IO_EndTrick => "EndTrick"
  | IO_Beg => "Beg"
  | IO_Stand => "Stand"
  | IO_GiveOne => "GiveOne"
  | IO_Deal => "Deal"
  | IO_RunPack => "RunPack"
  | IO_DealAgain => "DealAgain"
  | IO_LeaveGame => "LeaveGame"
  | IO_PlayAgain(ioUsername, _ioClientSettings) => {j|PlayAgain($ioUsername)|j}
  | IO_Rematch => "Rematch"
  | IO_Substitute(ioUsername, _ioClientSettings) => {j|Substitute($ioUsername)|j}
  | IO_PrivateToPublic => "PrivateToPublic"
  | IO_SelectPartner => "SelectPartner"
  | IO_StartGameNow => "SelectPartner"
  ;

type serverToClient =
  | SetState(ioClientState)
  | ShowToast(ioToast)
  | Reset
  | AckOk
  | AckError(string);

type ack = serverToClient => unit
