[%%debugger.chrome];
open AppPrelude;

[%%raw "require('dotenv').config()"]
[@bs.val] external nodeEnv: string = "process.env.NODE_ENV";
[@bs.val] external httpPortEnv: Js.Nullable.t(string) = "process.env.allfours_port";
[@bs.val] external adminPasswordEnv: Js.Nullable.t(string) = "process.env.allfours_admin_password";

[@bs.module] external nanoid: unit => string = "";

let logger = appLogger.makeChild({"_module": "Server"});

let app = Express.express();

Express.App.useOnPath(
  app,
  ~path="/",
  Express.Static.(make("./build", defaultOptions()) |> asMiddleware),
);

Express.App.useOnPath(
  app,
  ~path="/static",
  Express.Static.(make("./static", defaultOptions()) |> asMiddleware),
);

let http = Http.create(app);

let io =
  SocketServer.createWithHttpAndOption(http, SocketServer.makeOptions(~pingInterval=55000, ()));
let ns = SocketServer.Ns.of_(io, "/");

// let getKeysToRooms = () =>
//   SocketServer.Ns.getAdapter(ns) |> Obj.magic |> BsSocketExtra.Adapter.rooms;

module StringMap = Belt.Map.String;

let getGameStats = () => {
  let {ServerState.db_game, db_public_games_created} = ServerStore.getState();
  {
    "games-active": StringMap.valuesToArray(db_game) |> Array.length,
    "public-games-since-epoch": db_public_games_created,
  };
};










// let updateClientStates = gameState => {
//   gameState
//   ->buildSocketStatePairs
//   ->Belt.List.forEach(((socket, clientState)) =>
//       switch (socket) {
//       | None => ()
//       | Some(socket) =>
//         updateClientState(socket, clientState);

//         switch (Game.maybeGetSocketPlayer(socket, gameState)) {
//         | None => ()
//         | Some(playerId) =>
//           let notis =
//             List.filter(
//               noti => noti.Noti.noti_recipient == playerId,
//               gameState.notis,
//             );
//           let msg: SocketMessages.serverToClient =
//             AddNotis(notis |> ClientGame.notis_encode |> Js.Json.stringify);
//           SockServ.Socket.emit(socket, msg);
//         };
//       }
//     );

//    
//   // Clear notifications - All pending notifications for each client should have
//   // been delivered at this point. Go ahead and clear them.
//   
//   let gameState' = GameReducer.reduce(ClearNotis, gameState);

//   ServerStore.dispatch(ReplaceGame(gameState'.game_id, gameState'))
//   // StringMap.set(roomKey_gameState, Game.stringOfGameId(gameState'.game_id), gameState');
// };

let actionOfIO_Action: SocketMessages.clientToServer => GameReducer.action =
  fun
  | IO_JoinGame(_)
  | IO_StartPrivateGame(_)
  | IO_JoinPrivateGame(_)
  | IO_LeaveGame
  | IO_PlayAgain(_)
  | IO_Substitute(_) => Noop
  | IO_PlayCard(ioPlayerId, ioCard) => {
      switch (Player.id_decode(ioPlayerId |> Js.Json.parseExn)) {
      | Belt.Result.Error(_) => Noop
      | Belt.Result.Ok(playerId) =>
        switch (Card.t_decode(ioCard |> Js.Json.parseExn)) {
        | Belt.Result.Error(_) => Noop
        | Belt.Result.Ok(card) => PlayCard(playerId, card)
        }
      };
    }
  | IO_EndTrick => EndTrick
  | IO_Beg => Beg
  | IO_Stand => Stand
  | IO_GiveOne => GiveOne
  | IO_Deal => Deal
  | IO_RunPack => RunPack
  | IO_DealAgain => DealAgain
  | IO_CheatPoints(ioTeamId, points) => {
    switch(ioTeamId |> Js.Json.parseExn |> Team.id_decode){
      | Belt.Result.Error(_) => Noop
      | Belt.Result.Ok(teamId) => CheatPoints(teamId, points)
    };
    
  };

/* Apply the reducer action, save the updated game (in memory) and emit io
   that should cause clients to update. */

// let mutateAndPublish = (action, roomKey_gameState, stateForAction:Game.state ) => {
//   let stateAftAction = GameReducer.reduce(action, stateForAction);
//   let key = SharedGame.stringOfGameId(stateAftAction.Game.game_id);
//   StringMap.set(roomKey_gameState, key, stateAftAction);
//   updateClientStates(stateAftAction);
// };

// let leaveGame = socket => {
//   let logger =
//     logger.makeChild({"_context": "leaveGame", "socketId": SockServ.Socket.getId(socket)});
//   switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
//   | None => ()
//   | Some({maybeRoomKey}) =>
//     switch (maybeRoomKey) {
//     | None => ()
//     | Some(roomKey) =>
//       switch (StringMap.get(roomKey_gameState, roomKey)) {
//       | None => ()
//       | Some(gameForLeaving) =>
//         switch (Game.maybeGetSocketPlayer(socket, gameForLeaving)) {
//         | None => ()
//         | Some(playerId) =>
//           logger.debug2(
//             {
//               "player": {
//                 "username": Quad.select(playerId, player => player.Game.pla_name, gameForLeaving.players),
//                 "playerId": Player.stringOfId(playerId),
//               },

//               "game": Game.debugOfState(gameForLeaving),
//             },
//             "Removing player from game.",
//           );
//           My.Global.clearMaybeTimeout(gameForLeaving.maybeKickTimeoutId);
//           let gameAftLeaving = GameReducer.reduce(LeaveGame(playerId), gameForLeaving)
//           if (Game.playerCount(gameAftLeaving) == 0) {
//             StringMap.remove(roomKey_gameState, roomKey);
//           } else {
//             StringMap.set(roomKey_gameState, roomKey, gameAftLeaving);
//             updateClientStates(gameAftLeaving);

//             if (gameAftLeaving->Game.isPublic && gameAftLeaving.phase->Game.isFindSubsPhase) {
//               // forall public games in FindPlayersPhase, inform the players that they can
//               //   choose to become a substitute player in an existing public game
//               StringMap.valuesToArray(roomKey_gameState)
//               |> Belt.Array.keep(_, game => game->Game.isPublic && game.phase->Game.isFindPlayersPhase)
//               |> Belt.Array.forEach( _, mutateAndPublish(GameReducer.UpdateSubbing(true), roomKey_gameState),
//                  );
//             };
//           };

//           SockServ.Socket.emit(socket, SocketMessages.Reset);
//         }
//       }
//     }
//   };
// };

// let kickPlayer = (player) => {
//   switch(player.Game.sock_id_maybe){
//     | None => ()
//     | Some(sock_id) => ServerStore.dispatch @@ RemovePlayerBySocket(sock_id);
//   }
// };

// let reconcileKickTimeout = (prevGameState, nextGameState) => {
//   let prevMaybeActivePlayer = ActivePlayer.find(prevGameState.Game.phase, prevGameState.dealer);
//   let nextMaybeActivePlayer = ActivePlayer.find(nextGameState.Game.phase, nextGameState.dealer);
//   if (prevMaybeActivePlayer != nextMaybeActivePlayer) {
//     switch (nextMaybeActivePlayer) {
//     | None =>
//       //clear the previous timeout
//       My.Global.clearMaybeTimeout(prevGameState.maybeKickTimeoutId);
//       nextGameState;
//     | Some(nextActivePlayer) =>
//       // clear the prev timeout
//       My.Global.clearMaybeTimeout(prevGameState.maybeKickTimeoutId);
//       // and setup a new one for the new active player
//       let playerToKick = Quad.get(nextActivePlayer.id, nextGameState.players);

//       let maybeKickTimeoutId =
//         Some(
//           Js.Global.setTimeout(
//             () => kickPlayer(playerToKick),
//             SharedGame.settings.kickPlayerMillis,
//           ),
//         );

//       {...nextGameState, maybeKickTimeoutId};
//     };
//   } else {
//     nextGameState;
//   };
// };

// let joinGame = (socket, username, clientSettings) => {
//   let socketId = SockServ.Socket.(socket->getId);
//   let logger = logger.makeChild({"_context": "joinGame", "socketId": socketId});

  // let isPublicGame = gameState =>
  //   switch (gameState.Game.game_id) {
  //   | Public(_) => true
  //   | _ => false
  //   };
  // let {ServerState.db_game, db_player_game} = ServerStore.getState();

  // let publicGames = StringMap.valuesToArray(db_game)
  //   |> Belt.Array.keep(_, Game.isPublic);
  // let publicGamesFindingPlayers = Belt.Array.keep(publicGames, game => game.phase->Game.isFindPlayersPhase);
  // let publicGamesFindingSubs  = Belt.Array.keep(publicGames, game => game.phase->Game.isFindSubsPhase);

  /** 
    Lag on the client or other issues might cause the player to try joining multiple
    times. When that happens just remind him which game he's in.
  */
  // let maybeGetPlayerGame = (socket, gameStates) => {
  //   switch (
  //     Belt.Array.keep(gameStates, gameState =>
  //       Quad.exists(player => player.Game.pla_socket == Some(socket), gameState.Game.players)
  //     )
  //     |> Array.to_list
  //   ) {
  //   | [] => None
  //   | [gameState, ..._rest] => Some(gameState)
  //   };
  // };
  
  // switch(maybeGetPlayerGame(socket, publicGames)){
  // | Some(gameState) => 
  //   let maybePlayerIdAndData = 
  //     Quad.getPairWhere(player => player.Game.pla_socket == Some(socket), gameState.Game.players);
  //   switch(maybePlayerIdAndData){
  //   | None => 
  //     logger.warn2(
  //       Game.debugOfState(gameState),
  //       "Found an existing game and expected Some player to have this socketId but got None.",
  //     );
  //   | Some((playerId, _)) =>

//   switch(db_player_game->StringMap.get(socketId)){
//   | Some({sock_id, player_id, game_id})  => 
//     switch(db_game->StringMap.get(game_id->Game.stringOfGameId)){
//     | None => ()
//     | Some(gameState) =>
//       let (_, playerPhase) = Game.decidePlayerPhase(gameState.phase, gameState.dealer, player_id)
//       let clientState = buildClientState(gameState, player_id, playerPhase);
//       updateClientState(socket, clientState)
//     }
//   | None => 

//     let gameForJoining =
//       switch (publicGamesFindingPlayers |> Array.to_list) {
//       | [] => {
//           ...Game.initialState(),
//           game_id: Public(nextGameId() |> string_of_int),
//           phase: FindPlayersPhase(3, Array.length(publicGamesFindingSubs) == 0 ? false : true),
//         }
//       | [gameState, ..._rest] => gameState
//       };
    
//     let roomKey = Game.stringOfGameId(gameForJoining.game_id);

//     let socket = SockServ.Socket.join(socket, roomKey);
//     // StringMap.set(
//     //   socketId_playerData,
//     //   socketId,
//     //   {maybeRoomKey: Some(roomKey)},
//     // );

//     let playerId = Game.findEmptySeat(gameForJoining) |> Js.Option.getExn; // #unsafe #todo Handle attempt to join a full room or fix to ensure that unfilled room was actually unfilled
//     let pla_name = username == "" ? Player.stringOfId(playerId) : username;
//     let gameAftJoining = {
//       ...gameForJoining,
//       players:
//         Quad.update(
//           playerId,
//           x => Game.{...x, pla_name, pla_socket: Some(socket)},
//           gameForJoining.players,
//         ),
//     };

//     let playerCount = Game.playerCount(gameAftJoining);
//     let playersNeeded = 4 - playerCount;

//     let phaseAftPhasing =
//       switch (gameAftJoining.phase) {
//       | FindSubsPhase(_n, subPhase) =>
//         if (playersNeeded == 0) {
//           subPhase;
//         } else {
//           FindSubsPhase(playersNeeded, subPhase);
//         }

//       | FindPlayersPhase(_n, canSub) =>
//         playersNeeded == 0 ? DealPhase : FindPlayersPhase(playersNeeded, canSub)
//       | otherPhase => otherPhase
//       };

//     if (gameAftJoining.phase->Game.isFindSubsPhase && not(phaseAftPhasing->Game.isFindSubsPhase)) {
       
//       // This may be the last game to exit the FindSubPhase Any games that are in the
//       // FindPlayersPhase needs to disable the option for subbing.
      
//       let canSub =
//         publicGames |> Belt.Array.some(_, gameState => Game.isFindSubsPhase(gameState.phase));

//       if (!canSub) {
//         Belt.Array.keep(publicGames, gameState => Game.isFindPlayersPhase(gameState.phase))
//         |> Belt.Array.forEach(
//              _,
//              mutateAndPublish(GameReducer.UpdateSubbing(false), roomKey_gameState),
//            );
//       };
//     };

//     let playerJoinedNotis =
//       Noti.playerBroadcast(
//         ~from=playerId,
//         ~msg=Noti.Text(pla_name ++ " joined the game."),
//         ~level=Noti.Success,
//         (),
//       );

//     let gameAftUpdating = {
//       ...gameAftJoining,
//       phase: phaseAftPhasing,
//       notis: gameAftJoining.notis @ playerJoinedNotis,
//     };

//     let gameAftKickReconciling = reconcileKickTimeout(gameForJoining, gameAftUpdating);

//     logger.info2(Game.debugOfState(gameAftKickReconciling), "Saving game state." );

//     StringMap.set(roomKey_gameState, roomKey, gameAftKickReconciling);
//     updateClientStates(gameAftKickReconciling);
//   }
// };

// let joinPublicGame = (socket, username) => {
//   let sock_id = socket->SockServ.Socket.getId;
//   ServerStore.dispatch(AttachPublicPlayer(sock_id, socket, username));
//   let (db_game, db_player_game)
// }

// let rec makePrivateGame = (socket, username, clientSettings) => {
//   // create a random ID made from the string of 4 cards picked from the deck
//   let strId =
//     Deck.make()
//     |> Deck.shuffle
//     |> Belt.List.take(_, 4)
//     |> Js.Option.getExn
//     |> List.map(Card.codeOfCard)
//     |> Belt.List.toArray
//     |> Js.Array.joinWith(" ");

//   Js.log2("gameId = ", strId);

//   // Ensure this id is unique in the list of all games
//   // If the id is a duplicate 
//   let gameIdExists = (searchId, gameState) =>
//     switch (gameState.Game.game_id) {
//     | Public(_) => false
//     | Private(id) => searchId == id
//     };

//   let gameStates = StringMap.valuesToArray(roomKey_gameState);
//   if (Belt.Array.some(gameStates, gameIdExists(strId))) {
//     makePrivateGame(socket, username, clientSettings);
//   } else {
//     Game.{...initialState(), game_id: Private(strId)};
//   };
// };

// type gameReducerResult('state) = NoUpdate | Update('state);

// let joinGame2 = (socket, username, gameState) => {
//   let socketId = SockServ.Socket.getId(socket);
//   let str_game_id = Game.(stringOfGameId(gameState.game_id));
//   switch (Game.findEmptySeat(gameState)) {
//   | None => NoUpdate
//   | Some(playerId) =>
//     let pla_name = username == "" ? Player.stringOfId(playerId) : username;
//     let socket = SockServ.Socket.join(socket, str_game_id);
//     StringMap.set(socketId_playerData, socketId, {maybeRoomKey: Some(str_game_id)});

//     let players =
//       Quad.update(
//         playerId,
//         player => Game.{...player, pla_name, pla_socket: Some(socket)},
//         gameState.players,
//       );

//     let playersNeeded = 4 - Game.countPlayers(players);
//     let phase =
//       switch (gameState.phase) {
//       | FindSubsPhase(_n, subPhase) =>
//         playersNeeded == 0 ? subPhase : FindSubsPhase(playersNeeded, subPhase)
//       | FindPlayersPhase(_n, false) =>
//         playersNeeded == 0 ? DealPhase : FindPlayersPhase(playersNeeded, false)
//       | otherPhase => otherPhase
//       };

//     Update({...gameState, players, phase});
//   };
// };


let onSocketDisconnect = socket =>
  SocketServer.Socket.onDisconnect(
    socket,
    () => {
      /** Note: by this time socketio has already removed the socket from the room.
        Also, during testing I noticed that refreshing the app in chrome rapidly multiple times
        sometimes results in socketio not immediately detecting a disconnect. However it 
        seems to triage after about a minute(?) so there is a delay between when you expect to 
        see a disconnect and when socketio finally fires this disconnect handler.
        
        The result is that there may be open game rooms holding sockets that are effectively dead.*/

      let socketId = SocketServer.Socket.getId(socket);
      let logger = logger.makeChild({"_context": "socket-onDisconnect", "socketId": socketId});

      logger.info("Socket disconnected.");
      // leaveGame(socket);
      ServerStore.dispatch(RemovePlayerBySocket(socketId));
      SocketServer.Store.dispatch(RemoveSocket(socket));
      // StringMap.remove(socketId_playerData, socket->SockServ.Socket.getId);
      logger.info2(getGameStats(), "Game stats:");
    },
  );

SocketServer.onConnect(
  io,
  socket => {
    let sock_id = SocketServer.Socket.getId(socket);
    let logger = logger.makeChild({"_context": "socket-onconnect", "sock_id": sock_id});
    SocketServer.Store.dispatch(AddSocket(socket));
    logger.info("Socket connected");
    onSocketDisconnect(socket);
    SocketServer.Socket.emit(socket, SocketMessages.Reset);

    SocketServer.Socket.on(socket, io => {
      let stringOfEvent = SocketMessages.stringOfClientToServer(io);
      let logger = logger.makeChild({"_context": "socket-onevent", "event": stringOfEvent});
      logger.debug2("Handling `%s` event. ", stringOfEvent);
      switch (io) {
      | IO_StartPrivateGame(username, _ioClientSettingsJson) =>
        // let clientSettings =
        //   decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
        let gameState = Game.initPrivateGame();
        // let gameState = makePrivateGame(socket, username, clientSettings);
        // let str_game_id = Game.(stringOfGameId(gameState.game_id));
        ServerStore.dispatch(AddGame(gameState));
        ServerStore.dispatch(AttachPlayer(gameState.game_id, sock_id, username));
        // switch (joinGame2(socket, username, gameState)) {
        // | NoUpdate => logger.warn("Failed to join new private game")
        // | Update(gameState) =>
        //   logger.info2(Game.debugOfState(gameState), "Saving game state and updating clients.");

        //   StringMap.set(roomKey_gameState, str_game_id, gameState);
        //   updateClientStates(gameState);
          logger.info2(getGameStats(), "Game stats:");
        // };
      | IO_JoinPrivateGame(_inviteCode, _username, _ioClientSettings) => ()
      | IO_JoinGame(username, _ioClientSettingsJson) =>
        ServerStore.dispatch(AttachPublicPlayer(sock_id, username));
        // let gameState = switch(ServerStore.getGamesWhere(~privacy=Public, ~simplePhase=FindPlayersPhase, ())){
        // | [] =>
        //   let gameState = {...Game.initialState(), game_id: Public(nextGameId()->string_of_int)};
        //   // let gameState = makePrivateGame(socket, username, clientSettings);
        //   // let str_game_id = Game.(stringOfGameId(gameState.game_id));
        //   ServerStore.dispatch(AddGame(gameState));
        //   gameState
        // | [gameState, ..._rest] => gameState
        // }
        // ServerStore.dispatch(AttachPlayer(gameState.game_id, sock_id, username));
        // let clientSettings =
        //   decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);

        // joinGame(socket, username, clientSettings);
        logger.info2(getGameStats(), "Game stats:");
      | IO_LeaveGame => 
        // leaveGame(socket);
        ServerStore.dispatch(RemovePlayerBySocket(sock_id));
        // SocketServer.Store.dispatch(RemoveSocket(socket));
        logger.info2(getGameStats(), "Game stats:");
      | IO_PlayAgain(username, _ioClientSettingsJson) =>
        switch(ServerStore.getGameBySocket(sock_id)){
        | None => ()
        | Some(gameState) => 
          let newGameState = switch(gameState.game_id){
          | Public(_) => Game.initialState()
          | Private(_) => Game.initPrivateGame()
          }

          ServerStore.dispatchMany([
            RemovePlayerBySocket(sock_id),
            AddGame(newGameState),
            AttachPlayer(newGameState.game_id, sock_id, username)
          ]);
        }
        // leaveGame(socket);
        // let clientSettings =
        //   decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
        // joinGame(socket, username, clientSettings);
        logger.info2(getGameStats(), "Game stats:");
      | IO_Substitute(username) => 
        // let publicGames =
        //   StringMap.valuesToArray(roomKey_gameState) |> Belt.Array.keep(_, Game.isPublic);
        // switch (
        //   publicGames |> Belt.Array.keep(_, game => game.phase->Game.isFindSubsPhase) |> Array.to_list
        // ) {
        // switch(ServerStore.getGamesWhere(~privacy=Public, ~phase=FindSubsPhase, ())){
        // | [] => ()
        // | [gameForSub, ..._rest] =>
          // ServerStore.dispatch(RemovePlayerBySocket(sock_id));
          // ServerStore.dispatch(AttachPlayer(gameForSub.game_id, sock_id, username))
          // leaveGame(socket);
          ServerStore.dispatch(AttachSubstitute(sock_id, username));
          // switch (joinGame2(socket, username, gameForJoining)) {
          // | NoUpdate => logger.warn("Removed player from game but then failed to join as substitute.")
          // | Update(gameAftJoining) =>
          //   let gameAftKickReconciling = reconcileKickTimeout(gameForJoining, gameAftJoining);
          //   mutateAndPublish(GameReducer.Noop, roomKey_gameState, gameAftKickReconciling);
          //    
          //   //  If no game is left in the FindSubsPhase after this, 
          //   //  disable subbing for any players in the FindPlayersPhase.
          //   
          //   StringMap.valuesToArray(roomKey_gameState)
          //   |> Belt.Array.some(_, game => game->Game.isPublic && game.phase->Game.isFindSubsPhase)
          //     ? ()
          //     : StringMap.valuesToArray(roomKey_gameState)
          //       |> Belt.Array.keep(_, game => game->Game.isPublic && game.phase->Game.isFindPlayersPhase)
          //       |> Belt.Array.forEach(
          //            _,
          //            mutateAndPublish(GameReducer.UpdateSubbing(false), roomKey_gameState),
          //          );
          // };
        // };
      | ioAction =>
        // let roomKey =
        //   switch (StringMap.get(socketId_playerData, socket->SocketServer.Socket.getId)) {
        //   | None => ""
        //   | Some({maybeRoomKey}) => Js.Option.getWithDefault("", maybeRoomKey)
        //   };
        // switch (StringMap.get(roomKey_gameState, roomKey)) {
        // | None => ()
        // | Some(prevGameState) =>
          // let game_id_str = Game.stringOfGameId(prevGameState.game_id);
          let action = ioAction |> actionOfIO_Action;
          
          // let nextGameState = GameReducer.reduce(action, prevGameState);

          // let nextGameState = reconcileKickTimeout(prevGameState, nextGameState);

          ServerStore.dispatch(UpdateGameBySocket(sock_id, action));
          // let isEndTrick =
          //   Quad.map(
          //     player => Js.Option.isSome(player.Game.pla_card) ? true : false,
          //     nextGameState.players,
          //   )
          //   |> Quad.foldLeftUntil(
          //        (iPlayed, wePlayed) => wePlayed && iPlayed,
          //        wePlayed => !wePlayed,
          //        true,
          //      );

          // if (isEndTrick) {
          //   let advanceRound = () => {
          //     switch (StringMap.get(roomKey_gameState, roomKey)) {
          //     | None => ()
          //     | Some(prevGameState) =>
          //       let nextGameState = GameReducer.reduce(AdvanceRound, prevGameState);
          //       let nextGameState = reconcileKickTimeout(prevGameState, nextGameState);
          //       StringMap.set(roomKey_gameState, roomKey, nextGameState);
          //       updateClientStates(nextGameState);
          //     };
          //   };

          //   Js.Global.setTimeout(advanceRound, 2750) |> ignore;
          // };

          // logger.info2(Game.debugOfState(nextGameState), "Saving game state" );
          // StringMap.set(roomKey_gameState, roomKey, nextGameState);
          // updateClientStates(nextGameState);
        // };
      }
    });
    SocketServer.Socket.onWithAck(
      socket,
      (io, ack) => {
        let stringOfEvent = SocketMessages.stringOfClientToServer(io);
        let logger =
          logger.makeChild({"_context": "socket-onevent-with-ack", "event": stringOfEvent});
        logger.debug2("Handling `%s` event. ", stringOfEvent);
        switch (io) {
        | IO_JoinPrivateGame(inviteCode, username, _ioClientSettings) =>
          logger.info("Invite code: " ++ inviteCode);
          ServerStore.dispatch(AttachPrivatePlayer(sock_id, username, inviteCode, ack))

        | _ => logger.warn("Ignoring socket message")
        };
      },
    );
  },
);

switch (Js.Nullable.toOption(adminPasswordEnv)) {
| None =>
  failwith(
    "An environment variable 'allfours_admin_password' must be provided.\nThis defines the password for the admin dashboard.",
  )
| Some(adminPassword) =>
  Express.App.get(app, ~path="/dashboard", Raw.authMiddleware("admin", adminPassword));
  Express.App.get(
    app,
    ~path="/dashboard",
    Express.Middleware.from((_next, _req) => {
      let {ServerState.db_game, db_public_games_created} = ServerStore.getState();
      let gameStates = StringMap.valuesToArray(db_game);
      <div>
        <div>
          <span> {"Games since server started: " |> ReasonReact.string} </span>
          <span> {db_public_games_created |> string_of_int |> ReasonReact.string} </span>
        </div>
        <div>
          <span> {"Currently Active games: " |> ReasonReact.string} </span>
          <span> {Array.length(gameStates) |> string_of_int |> ReasonReact.string} </span>
        </div>
        <DashView gameStates />
      </div>
      |> ReactDOMServerRe.renderToStaticMarkup
      |> Express.Response.sendString;
    }),
  );
};

/** 
 Add catchall for requests that should be handled by react's router
 (ReasonReactRouter) on the client side
 */
Express.App.get(app, ~path="/*") @@
Express.Middleware.from((_next, _req, res) =>
  Express.Response.redirect("/", res)
);

let httpPort = Js.Nullable.toOption(httpPortEnv)
 |> Js.Option.getWithDefault("3000")
 |> int_of_string;

Http.listen(http, httpPort, () => print_endline("Listening on *:" ++ string_of_int(httpPort)));
