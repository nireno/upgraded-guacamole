open AppPrelude;
type ioPlayerId = str_json
type ioTeamId = str_json
type ioCard = str_json
type ioUsername = string
type ioClientState = str_json
type ioClientNotis = str_json

type clientToServer =
  | IO_JoinGame(ioUsername)
  | IO_PlayCard(ioPlayerId, ioCard)
  | IO_EndTrick
  | IO_NewRound
  | IO_EndRound
  | IO_Beg
  | IO_Stand
  | IO_GiveOne
  | IO_Deal
  | IO_RunPack
  | IO_DealAgain
  | IO_LeaveGame
  | IO_PlayAgain
  | IO_CheatPoints(ioTeamId, int);

type serverToClient =
  | SetState(ioClientState)
  | AddNotis(ioClientNotis);
