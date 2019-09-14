open AppPrelude;

type attachPlayerData = {
  game_id: Game.game_id,
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
};

type attachPublicPlayerData = {
  sock_id: string,
  client_username: string,
  ack: SocketMessages.ack,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
};

type attachPrivatePlayerData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
  invite_code: string,
  ack: SocketMessages.ack,
};

type createPrivateGameData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
};

type attachSubstituteData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
};

type event = 
  | AddGame(Game.state)
  | AddPlayerGame(sock_id, Player.id, Game.game_id)
  | RemoveGame(Game.game_id)
  | ReplaceGame(Game.game_id, Game.state)
  | AttachPlayer(attachPlayerData)
  | AttachPublicPlayer(attachPublicPlayerData)
  | CreatePrivateGame(createPrivateGameData)
  | AttachPrivatePlayer(attachPrivatePlayerData)
  | RemovePlayerBySocket(sock_id)
  | AttachSubstitute(attachSubstituteData)
  | KickActivePlayer(Game.game_id)
  | InsertKickTimeoutId(Game.game_id, Js.Global.timeoutId)
  | DeleteKickTimeoutId(Game.game_id)
  | UpdateGame(Game.game_id, GameReducer.action)
  | UpdateGameBySocket(sock_id, GameReducer.action)
  | TriggerEffects(list(effect))
  | ReconcileSubstitution
  | IdleWithTimeout(Game.game_id, Timer.timeout, Game.idleReason)
  | Rematch(sock_id)
  | RotateGuests(sock_id)
  | StartGameNow(sock_id) //short circuits the usual delay before game starts
  | PrivateToPublic(sock_id)
and effect = 
  // affecting socketio
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(Game.game_id)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list(clientToast))
  // affecting node timers
  | ResetKickTimeout(Game.game_id)
  | ClearKickTimeout(Game.game_id)
  // | DelayThenAdvanceRound(Game.game_id)
  | IdleThenUpdateGame(idleThenUpdateGame)
  // | DelayedEvent(delayedEvent)
and clientToast = {
  sock_id,
  toast: Noti.t,
} 
and idleThenUpdateGame = {
  game_id: Game.game_id,
  game_reducer_action: GameReducer.action,
  idle_milliseconds: int,
};
// and delayedEvent = {
//   event: event,
//   delay_milliseconds: int,
// };