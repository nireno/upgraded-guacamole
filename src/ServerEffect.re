open AppPrelude;

type clientToast = {
  sock_id,
  toast: Noti.t,
};

type effect = 
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(Game.game_id)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list(clientToast))
  | ResetKickTimeout(Game.game_id)
  | ClearKickTimeout(Game.game_id)
  | DelayThenAdvanceRound(Game.game_id);
