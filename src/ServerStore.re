open AppPrelude;
open Belt;

let logger = appLogger.makeChild({"_context": "ServerStore"});

let store: ref(ServerState.db) = ref(ServerState.empty());


let getState: unit => ServerState.db = () => store^;

let getGameBySocket: sock_id => option(Game.state) =
  sock_id => ServerState.getGameBySocket(sock_id, getState());


let getGamesWhere: (~phase: Game.Filter.phase=?, ~privacy: Game.Filter.privacy=?, unit) => list(Game.state) =
  (~phase=?, ~privacy=PrivateOrPublic, ()) => {
    ServerState.getGamesWhere(~phase?, ~privacy, getState());
  };

let rec dispatch: ServerState.msg => unit =
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
and perform: (ServerState.db, ServerEffect.effect) => unit =
  ({db_game} as db, effect) => {
    switch (effect) {
    | EmitClientState(sock_id, clientState) =>
      // let {db_socket} = ServerStore.getState();
      // let socket = db_socket->StringMap.get(sock_id);
      let clientStateJson = clientState |> ClientGame.state_encode; // #unsafe
      let msg: SocketMessages.serverToClient = SetState(clientStateJson |> Js.Json.stringify);
      SocketServer.emit(msg, sock_id);
    
    | EmitStateByGame(game_id) => 
      switch (db_game->StringMap.get(game_id->Game.stringOfGameId)) {
      | None => 
        logger.debug("EmitStatByGame: game not found")
      | Some(gameState) =>
        gameState
        ->ServerState.buildSocketStatePairs
        ->Belt.List.forEach(((sock_id_maybe, clientState)) =>
            switch (sock_id_maybe) {
            | None => ()
            | Some(sock_id) => perform(db, ServerEffect.EmitClientState(sock_id, clientState))
            }
          )
      };

    | EmitClientToasts(clientToasts) =>
      clientToasts->List.forEach(clientToast => {
        let msg: SocketMessages.serverToClient =
          ShowToast(clientToast.toast |> Noti.t_encode |> Js.Json.stringify);
        SocketServer.emit(msg, clientToast.sock_id);
      })

    | ResetKickTimeout(game_id) =>
      perform(db, ServerEffect.ClearKickTimeout(game_id));
      let timeoutId =
        Js.Global.setTimeout(
          () => dispatch(KickActivePlayer(game_id)),
          SharedGame.settings.kickPlayerMillis,
        );
      dispatch(InsertKickTimeoutId(game_id, timeoutId))

    | ClearKickTimeout(game_id) =>
      switch(db_game->StringMap.get(game_id->Game.stringOfGameId)){
      | None => ()
      | Some(gameState) => 
        switch(gameState.maybeKickTimeoutId){
        | None => ()
        | Some(kickTimeoutId) =>
          Js.Global.clearTimeout(kickTimeoutId)
          dispatch(DeleteKickTimeoutId(game_id));
        }
      }

    | DelayThenAdvanceRound(game_id) =>
      let timeout = Timer.startTimeout(() => dispatch(UpdateGame(game_id, AdvanceRound)), 2750);
      dispatch(IdleWithTimeout(game_id, timeout));

    | EmitAck(ack, msg) => ack(msg)
    | ResetClient(sock_id) => 
      SocketServer.emit(Reset, sock_id);
    };
  };

let dispatchMany: list(ServerState.msg) => unit = msgs => {
  msgs->List.forEach(dispatch);
}
