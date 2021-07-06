open AppPrelude

type attachPlayerData = {
  game_key: game_key,
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
}

type attachPublicPlayerData = {
  sock_id: string,
  client_username: string,
  ack: SocketMessages.ack,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
}

type attachPrivatePlayerData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
  invite_code: string,
  ack: SocketMessages.ack,
}

type createPrivateGameData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
}

type attachSubstituteData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
}

type rec event =
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
  | UpdateGame(game_key, Game.event)
  | UpdateGameBySocket(sock_id, Game.event)
  | TriggerEffects(list<effect>)
  | ReconcileSubstitution
  | Rematch(sock_id)
  | RotateGuests(sock_id)
  | PrivateToPublic(sock_id)
  | FireGameTimer(sock_id)
  | AddGameTimeout(addGameTimeoutContext)
  | RemoveGameTimeout(game_key)
  | PrivateGameStarted
  | PublicGameStarted
  | RelaySignal(game_key, PlayerSignal.t)
and effect =
  | EmitSignal(sock_id, Quad.id, PlayerSignal.t)
  | NotifyPlayer(game_key, Noti.t)
  // affecting socketio
  | ResetClient(sock_id)
  | EmitClientState(sock_id, ClientGame.state)
  | EmitStateByGame(game_key)
  | EmitAck(SocketMessages.serverToClient => unit, SocketMessages.serverToClient)
  | EmitClientToasts(list<clientToast>)
  // affecting node timers
  | CreateGameTimer(game_key, gameTimerType)
  | DiscardGameTimer(game_key)
and clientToast = {
  sock_id: sock_id,
  toast: Noti.t,
}
and idleThenUpdateGame = {
  game_key: game_key,
  game_reducer_action: Game.event,
  idle_milliseconds: int,
}
and transitionGameContext = {
  game_key: game_key,
  fromPhase: Game.phase,
  toPhase: Game.phase,
}
and addGameDelayedEventContext = {
  game_key: game_key,
  event: event,
  delay_milliseconds: int,
}
and addGameTimeoutContext = {
  game_key: game_key,
  timeout: Timer.timeout,
}
and gameTimerType =
  | DelayedGameEvent(Game.event, int /* delayMilliseconds */)
  | TransitionGameCountdown(Game.phase /* fromPhase */, Game.phase /* toPhase */)
  | KickInactiveClientCountdown

let debugOfEffect = x =>
  switch x {
  | ResetClient(_) => j`ResetClient`
  | EmitClientState(_, _) => j`EmitClientState`
  | EmitStateByGame(_) => j`EmitStateByGame`
  | EmitAck(_, _) => j`EmitAck`
  | EmitClientToasts(_) => j`EmitClientToasts`
  | NotifyPlayer(_) => j`NotifyPlayer`
  | CreateGameTimer(_) => j`CreateGameTimer`
  | DiscardGameTimer(_) => j`DiscardGameTimer`
  | EmitSignal(_) => j`EmitSignal`
  }
