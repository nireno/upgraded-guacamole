open AppPrelude;

type attachPlayerData = {
  game_key,
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
  | AddPlayerGame(sock_id, Player.id, game_key)
  | RemoveGame(game_key)
  | ReplaceGame(game_key, Game.state)
  | AttachPlayer(attachPlayerData)
  | AttachPublicPlayer(attachPublicPlayerData)
  | CreatePrivateGame(createPrivateGameData)
  | AttachPrivatePlayer(attachPrivatePlayerData)
  | RemovePlayerBySocket(sock_id)
  | AttachSubstitute(attachSubstituteData)
  | KickActivePlayer(game_key)
  | InsertKickTimeoutId(game_key, Js.Global.timeoutId)
  | DeleteKickTimeoutId(game_key)
  | UpdateGame(game_key, GameReducer.action)
  | UpdateGameBySocket(sock_id, GameReducer.action)
  | TriggerEffects(list(effect))
  | ReconcileSubstitution
  | IdleWithTimeout(game_key, Timer.timeout, Game.idleReason)
  | Rematch(sock_id)
  | RotateGuests(sock_id)
  | StartGameNow(sock_id) //short circuits the usual delay before game starts
  | PrivateToPublic(sock_id)
and effect = 
  // affecting socketio
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(game_key)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list(clientToast))
  // affecting node timers
  | ResetKickTimeout(game_key)
  | ClearKickTimeout(game_key)
  // | DelayThenAdvanceRound(Game.game_id)
  | IdleThenUpdateGame(idleThenUpdateGame)
  // | DelayedEvent(delayedEvent)
and clientToast = {
  sock_id,
  toast: Noti.t,
} 
and idleThenUpdateGame = {
  game_key,
  game_reducer_action: GameReducer.action,
  idle_milliseconds: int,
};
// and delayedEvent = {
//   event: event,
//   delay_milliseconds: int,
// };