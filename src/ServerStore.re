open AppPrelude;
open Belt;

let logger = appLogger.makeChild({"_context": "ServerStore"});

let store: ref(ServerState.db) = ref(ServerState.empty());


let getState: unit => ServerState.db = () => store^;

let getGameBySocket: sock_id => option(Game.state) =
  sock_id => ServerState.getGameBySocket(sock_id, getState());


let getGamesWhere: (~phase: Game.Filter.simplePhase=?, ~privacy: Game.Filter.privacy=?, unit) => list(Game.state) =
  (~phase=?, ~privacy=PrivateOrPublic, ()) => {
    ServerState.getGamesWhere(~phase?, ~privacy, getState());
  };

let rec dispatch: ServerEvent.event => unit =
  msg => {
    let state = getState();
    switch (ServerState.update(msg, state)) {
    | NoUpdate(_) => ()
    | Update(stateAftUpdate) => 
      store := stateAftUpdate
    | SideEffects(_, effects) => 
      List.forEach(effects, perform(getState()))
    | UpdateWithSideEffects(stateAftUpdate, effects) =>
      store := stateAftUpdate;
      effects->List.forEach(perform(stateAftUpdate));
    };
  }
and perform: (ServerState.db, ServerEvent.effect) => unit =
  ({db_game} as db, effect) => {
    switch (effect) {
    | EmitClientState(sock_id, clientState) =>
      // let {db_socket} = ServerStore.getState();
      // let socket = db_socket->StringMap.get(sock_id);
      let clientStateJson = clientState |> ClientGame.state_encode; // #unsafe
      let msg: SocketMessages.serverToClient = SetState(clientStateJson |> Js.Json.stringify);
      SocketServer.emit(msg, sock_id);
    
    | EmitStateByGame(game_key) => 
      switch (db_game->StringMap.get(game_key)) {
      | None => 
        logger.debug("EmitStatByGame: game not found")
      | Some(gameState) =>
        gameState
        ->ServerState.buildSocketStatePairs
        ->Belt.List.forEach(((sock_id_maybe, clientState)) =>
            switch (sock_id_maybe) {
            | None => ()
            | Some(sock_id) => perform(db, ServerEvent.EmitClientState(sock_id, clientState))
            }
          )
      };

    | EmitClientToasts(clientToasts) =>
      clientToasts->List.forEach(clientToast => {
        let msg: SocketMessages.serverToClient =
          ShowToast(clientToast.toast |> Noti.t_encode |> Js.Json.stringify);
        SocketServer.emit(msg, clientToast.sock_id);
      })

    | ResetKickTimeout(game_key) =>
      perform(db, ServerEvent.ClearKickTimeout(game_key));
      let timeoutId =
        Js.Global.setTimeout(
          () => dispatch(KickActivePlayer(game_key)),
          SharedGame.settings.kickPlayerMillis,
        );
      dispatch(InsertKickTimeoutId(game_key, timeoutId))

    | ClearKickTimeout(game_key) =>
      switch(db_game->StringMap.get(game_key)){
      | None => ()
      | Some(gameState) => 
        switch(gameState.maybeKickTimeoutId){
        | None => ()
        | Some(kickTimeoutId) =>
          Js.Global.clearTimeout(kickTimeoutId)
          dispatch(DeleteKickTimeoutId(game_key));
        }
      }

    | IdleThenUpdateGame({game_key, game_reducer_action, idle_milliseconds}) =>
      let timeout =
        Timer.startTimeout(
          () => dispatch(UpdateGame(game_key, game_reducer_action)),
          idle_milliseconds,
        );
      let idleReason = switch(game_reducer_action){
      | StartGame => Game.StartGameIdle
      | _ => UpdateGameIdle
      }
      dispatch(IdleWithTimeout(game_key, timeout,idleReason));

    | EmitAck(ack, msg) => ack(msg)
    | ResetClient(sock_id) => 
      SocketServer.emit(Reset, sock_id);
    // | DelayedEvent({event, delay_milliseconds}) =>
    //   /* TODO: I use DelayedEvent to add a countdown before the game starts. 
    //      At the moment, the timer it creates is unstoppable. So if a player leaves
    //      before the game starts the timer will still fire. This won't have any effect
    //      because the GameReducer event ensures the game is in a valid state before
    //      attempting to start the game. However it should be the case that, either the
    //      ServerStore or GameReducer should keep track of running timers and clear them
    //      if the game is no longer in a valid state to carry out the DelayedEvent.
    //   */

    //     Timer.startTimeout(
    //       () => dispatch(event),
    //       delay_milliseconds,
    //     ) |> ignore;
      
    };
  };

let dispatchMany: list(ServerEvent.event) => unit = msgs => {
  msgs->List.forEach(dispatch);
}
