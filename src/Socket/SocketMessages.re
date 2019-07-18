open AppPrelude;
type ioPlayerId = str_json
type ioTeamId = str_json
type ioCard = str_json
type ioInviteCode = string
type ioUsername = string
type ioClientState = str_json
type ioClientNotis = str_json
type ioClientSettings = str_json

type clientToServer =
  | IO_JoinGame(ioUsername, ioClientSettings)
  | IO_StartPrivateGame(ioUsername, ioClientSettings)
  | IO_JoinPrivateGame(ioInviteCode, ioUsername, ioClientSettings)
  | IO_PlayCard(ioPlayerId, ioCard)
  | IO_EndTrick
  | IO_NewRound
  | IO_Beg
  | IO_Stand
  | IO_GiveOne
  | IO_Deal
  | IO_RunPack
  | IO_DealAgain
  | IO_LeaveGame
  | IO_PlayAgain(ioUsername, ioClientSettings)
  | IO_Substitute(ioUsername)
  | IO_CheatPoints(ioTeamId, int);

let stringOfClientToServer = fun
  | IO_JoinGame(ioUsername, _) => "JoinGame(" ++ (ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")"
  | IO_StartPrivateGame(ioUsername, _) => "StartPrivateGame(" ++ (ioUsername == "" ? "--blank-username--" : ioUsername) ++ ")"
  | IO_JoinPrivateGame(inviteCode, username, _) => "JoinPrivateGame(" ++ inviteCode ++ ", " ++ (username == "" ? "--blank-username--" : username) ++ ")"
  | IO_PlayCard(ioPlayerId, ioCard) => {j|PlayCard($ioPlayerId, $ioCard)|j}
  | IO_EndTrick => "EndTrick"
  | IO_NewRound => "NewRound"
  | IO_Beg => "Beg"
  | IO_Stand => "Stand"
  | IO_GiveOne => "GiveOne"
  | IO_Deal => "Deal"
  | IO_RunPack => "RunPack"
  | IO_DealAgain => "DealAgain"
  | IO_LeaveGame => "LeaveGame"
  | IO_PlayAgain(ioUsername, _ioClientSettings) => {j|PlayAgain($ioUsername)|j}
  | IO_Substitute(ioUsername) => {j|Substitute($ioUsername)|j}
  | IO_CheatPoints(_ioTeamId, _int) => "CheatPoints";

type serverToClient =
  | SetState(ioClientState)
  | AddNotis(ioClientNotis)
  | Reset
  | AckOk
  | AckError(string);
