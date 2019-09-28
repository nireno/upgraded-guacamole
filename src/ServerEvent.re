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
  | UpdateGame(game_key, Game.event)
  | UpdateGameBySocket(sock_id, Game.event)
  | TriggerEffects(list(effect))
  | ReconcileSubstitution
  | IdleWithTimeout(game_key, Timer.timeout, Game.idleReason)
  | Rematch(sock_id)
  | RotateGuests(sock_id)
  | PrivateToPublic(sock_id)
  | FireGameTimer(sock_id)
  | AddGameTimeout(addGameTimeoutContext)
  | RemoveGameTimeout(game_key)
  | TransitionGame(transitionGameContext)
and effect = 
  | NotifyPlayer(game_key, Noti.t)
  // affecting socketio
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(game_key)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list(clientToast))
  // affecting node timers
  | IdleThenUpdateGame(idleThenUpdateGame)
  | AddDelayedEvent(addGameDelayedEventContext)
  | CreateGameTimer(game_key, gameTimerType)
  | DiscardGameTimer(game_key)
and clientToast = {
  sock_id,
  toast: Noti.t,
} 
and idleThenUpdateGame = {
  game_key,
  game_reducer_action: Game.event,
  idle_milliseconds: int,
}
and transitionGameContext = {
  game_key,
  fromPhase: Game.phase,
  toPhase: Game.phase,
}
and addGameDelayedEventContext = {
  game_key, 
  event: event,
  delay_milliseconds: int,
}
and addGameTimeoutContext = {
  game_key, 
  timeout: Timer.timeout
}
and gameTimerType = 
| AdvanceRoundDelay
| GameStartingCountdown
| KickInactiveClientCountdown
;

let debugOfEffect = fun
  | ResetClient(_) => {j|ResetClient|j}
  | EmitClientState(_, _) => {j|EmitClientState|j}
  | EmitStateByGame(_) => {j|EmitStateByGame|j}
  | EmitAck(_, _) => {j|EmitAck|j}
  | EmitClientToasts(_) => {j|EmitClientToasts|j}
  | IdleThenUpdateGame(_) => {j|IdleThenUpdateGame|j}
  | AddDelayedEvent(_) => {j|IdleThenUpdateGame|j}
  | NotifyPlayer(_) => {j|NotifyPlayer|j}
  | CreateGameTimer(_) => {j|CreateGameTimer|j}
  | DiscardGameTimer(_) => {j|DiscardGameTimer|j}
  ;