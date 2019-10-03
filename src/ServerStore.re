open AppPrelude;
open Belt;

let logger = appLogger.makeChild({"_context": "ServerStore"});

let store: ref(ServerState.db) = ref(ServerState.empty());


let getState: unit => ServerState.db = () => store^;

let getGameBySocket: sock_id => option(Game.state) =
  sock_id => ServerState.getGameBySocket(sock_id, getState());


// let getGamesWhere: (~phase: Game.Filter.simplePhase=?, ~privacy: Game.Filter.privacy=?, unit) => list(Game.state) =
//   (~phase=?, ~privacy=PrivateOrPublic, ()) => {
//     ServerState.getGamesWhere(~phase?, ~privacy, getState());
//   };

let rec dispatch: ServerEvent.event => unit =
  msg => {
    let state = getState();
    switch (ServerState.update(msg, state)) {
    | NoUpdate(_) => ()
    | Update(stateAftUpdate) => 
      store := stateAftUpdate
    | SideEffects(_, effects) => 
      // let effectNames = effects->Belt.List.map(effect => effect->ServerEvent.debugOfEffect);
      // logger.debug2("Performing effects", effectNames);
      List.forEach(effects, perform(getState()))
    | UpdateWithSideEffects(stateAftUpdate, effects) =>
      // let effectNames = effects->Belt.List.map(effect => effect->ServerEvent.debugOfEffect);
      // logger.debug2("Performing effects", effectNames);
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


    | EmitAck(ack, msg) => ack(msg)
    | ResetClient(sock_id) => 
      SocketServer.emit(Reset, sock_id);

    | NotifyPlayer(game_key, {noti_recipient: seat_id} as noti) =>
      switch(db_game->StringMap.get(game_key)){
      | None => ()
      | Some(game) => 
        switch(game.clients->Quad.get(seat_id, _)){
        | Connected({client_socket_id}) =>
          let msg: SocketMessages.serverToClient =
            ShowToast(noti |> Noti.t_encode |> Js.Json.stringify);
          SocketServer.emit(msg, client_socket_id);
        | _ => ()
        }
      }
    | CreateGameTimer(game_key, gameTimerType) =>
      switch(gameTimerType){
      | KickInactiveClientCountdown => 
        let kickTimer =
          Timer.startTimeout(
            () => {
              dispatch(KickActivePlayer(game_key))
            },
            SharedGame.settings.kickPlayerMillis,
          );
        dispatch(AddGameTimeout({game_key, timeout: kickTimer}));
      
      | TransitionGameCountdown(fromPhase, toPhase) =>
        dispatch(
          AddGameTimeout({
            game_key,
            timeout:
              Timer.startTimeout(
                () => dispatch(TransitionGame({game_key, fromPhase, toPhase})),
                SharedGame.settings.gameStartingCountdownSeconds->secondsToMillis,
              ),
          }),
        );

      | DelayedGameEvent(event, delayMilliseconds) =>
        dispatch(
          AddGameTimeout({
            game_key,
            timeout: Timer.startTimeout(() => dispatch(UpdateGame(game_key, event)), delayMilliseconds),
          }),
        );
      }
    | DiscardGameTimer(game_key) =>
      dispatch(RemoveGameTimeout(game_key));
    };
  }
  and dispatchMany: list(ServerEvent.event) => unit = msgs => {
  msgs->List.forEach(dispatch);
}
