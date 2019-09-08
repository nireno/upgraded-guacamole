open AppPrelude;

type clientToast = {
  sock_id,
  toast: Noti.t,
};

type idleThenUpdateGame = {
  game_id: Game.game_id,
  game_reducer_action: GameReducer.action,
  idle_milliseconds: int,
};

type effect = 
  // affecting socketio
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(Game.game_id)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list(clientToast))
  // affecting node timers
  | ResetKickTimeout(Game.game_id)
  | ClearKickTimeout(Game.game_id)
  | DelayThenAdvanceRound(Game.game_id)
  | IdleThenUpdateGame(idleThenUpdateGame);
