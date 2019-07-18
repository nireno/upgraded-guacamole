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

module SockServ = BsSocket.Server.Make(SocketMessages);
module Namespace = BsSocket.Namespace.Make(SocketMessages);

let http = Http.create(app);

let io = SockServ.createWithHttpAndOption(http, SockServ.makeOptions(~pingInterval=55000, ()));
let ns = Namespace.of_(io, "/");

let getKeysToRooms = () =>
  Namespace.getAdapter(ns) |> Obj.magic |> BsSocketExtra.Adapter.rooms;

let expectedGameCount = 50;
module StringMap = Belt.HashMap.String;

type playerData = {
  maybeRoomKey: option(string),
};

/** Map of player sockets to their respective game data */
let socketId_playerData: StringMap.t(playerData) = 
  StringMap.make(~hintSize=(expectedGameCount*4));

let nGamesCreated = ref(0);

let nextGameId = () => {
  nGamesCreated := nGamesCreated^ + 1;
  nGamesCreated^
};

/** Map of room keys to respective game states */
let roomKey_gameState: StringMap.t(Game.state) =
  StringMap.make(~hintSize=expectedGameCount);

let getGameStats = () => {
  {
    "games-active": StringMap.valuesToArray(roomKey_gameState) |> Array.length,
    "games-since-epoch": nGamesCreated^,
  };
};

let getPartnerInfo = (gameState, playerId) => {
  let partnerId = Player.getPartner(playerId);
  let partner = Quad.get(partnerId, gameState.Game.players);
  {
    ClientGame.cardsToDisplay:
      List.filter(
        card =>
          card.Card.rank == Card.Rank.Ace
          || card.rank == Card.Rank.King
          || card.rank == Card.Rank.Queen
          || card.rank == Card.Rank.Jack
          || card.rank == Card.Rank.Ten,
        partner.pla_hand,
      ),
    trumpCount:
      switch (gameState.maybeTrumpCard) {
      | None => 0
      | Some(trumpCard) =>
        partner.pla_hand |> List.filter(card => card.Card.suit == trumpCard.suit) |> List.length
      },
  };
};

let buildClientState = (gameState, player, playerPhase) => {
  let playerHand = Quad.get(player, gameState.Game.players).pla_hand;
  let handFacing =
    if (SharedGame.isFaceDownPhase(gameState.phase)) {
      player == gameState.dealer || player == gameState.leader 
        ? Hand.FaceUpHand(playerHand) 
        : Hand.FaceDownHand(List.length(playerHand));
    } else {
      Hand.FaceUpHand(playerHand);
    };

  let p1State = Quad.get(N1, gameState.players);
  let p2State = Quad.get(N2, gameState.players);
  let p3State = Quad.get(N3, gameState.players);
  let p4State = Quad.get(N4, gameState.players);
  ClientGame.{
    gameId: gameState.game_id,
    phase: playerPhase,
    gamePhase: gameState.phase,
    players: (
      {
        pla_name: p1State.pla_name,
        pla_card: p1State.pla_card
      },
      {
        pla_name: p2State.pla_name,
        pla_card: p2State.pla_card
      },
      {
        pla_name: p3State.pla_name,
        pla_card: p3State.pla_card
      },
      {
        pla_name: p4State.pla_name,
        pla_card: p4State.pla_card
      },
    ),
    teams: gameState.teams,
    me: player,
    maybePartnerInfo: switch(gameState.phase){
    | PlayerTurnPhase(_n) => Some(getPartnerInfo(gameState, player))
    | _ => None
    },
    myTricks: Quad.get(player, gameState.players).pla_tricks,
    dealer: gameState.dealer,
    leader: gameState.leader,
    handFacing,
    maybeTrumpCard: gameState.maybeTrumpCard,
    maybeLeadCard: gameState.maybeLeadCard,
    maybeTeamHigh: gameState.maybeTeamHigh,
    maybeTeamLow: gameState.maybeTeamLow,
    maybeTeamJack: gameState.maybeTeamJack,
    maybeTeamGame: gameState.maybeTeamGame,
  };
};

let buildSocketStatePairs: Game.state => list((option(BsSocket.Server.socketT),ClientGame.state )) = gameState => {
  let decidePlayerPhase = 
    Game.decidePlayerPhase(
      gameState.phase,
      gameState.dealer);

  let playerPhasePairs: list((Player.id, Player.phase)) = 
    [Quad.N1, N2, N3, N4] 
      -> Belt.List.map(p => p->decidePlayerPhase)

  let buildClientState = buildClientState(gameState);
  playerPhasePairs->Belt.List.map(
    ( (player, playerPhase) ) => 
    (Quad.select(player, x => Game.(x.pla_socket), gameState.players), buildClientState(player, playerPhase)))
}

let updateClientState = (socket, clientState) => {
  let msg: SocketMessages.serverToClient =
    SetState(
      clientState |> ClientGame.state_encode |> Js.Json.stringify // #unsafe
    );
  SockServ.Socket.emit(socket, msg);
};

let updateClientStates = gameState => {
  gameState
  ->buildSocketStatePairs
  ->Belt.List.forEach(((socket, clientState)) =>
      switch (socket) {
      | None => ()
      | Some(socket) =>
        updateClientState(socket, clientState);

        switch (Game.maybeGetSocketPlayer(socket, gameState)) {
        | None => ()
        | Some(playerId) =>
          let notis =
            List.filter(
              noti => noti.Noti.noti_recipient == playerId,
              gameState.notis,
            );
          let msg: SocketMessages.serverToClient =
            AddNotis(notis |> ClientGame.notis_encode |> Js.Json.stringify);
          SockServ.Socket.emit(socket, msg);
        };
      }
    );

  /* 
    Clear notifications - All pending notifications for each client should have
    been delivered at this point. Go ahead and clear them.
  */
  let gameState' = GameReducer.reduce(ClearNotis, gameState);

  StringMap.set(roomKey_gameState, Game.stringOfGameId(gameState'.game_id), gameState');
};

let actionOfIO_Action: SocketMessages.clientToServer => GameReducer.action =
  fun
  | IO_JoinGame(_)
  | IO_StartPrivateGame(_)
  | IO_JoinPrivateGame(_)
  | IO_LeaveGame
  | IO_PlayAgain(_) => Noop
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
  | IO_NewRound => NewRound
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
let mutateAndPublish = (action, roomKey_gameState, stateForAction:Game.state ) => {
  let stateAftAction = GameReducer.reduce(action, stateForAction);
  let key = SharedGame.stringOfGameId(stateAftAction.Game.game_id);
  StringMap.set(roomKey_gameState, key, stateAftAction);
  updateClientStates(stateAftAction);
};

let leaveGame = socket => {
  let logger =
    logger.makeChild({"_context": "leaveGame", "socketId": SockServ.Socket.getId(socket)});
  switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
  | None => ()
  | Some({maybeRoomKey}) =>
    switch (maybeRoomKey) {
    | None => ()
    | Some(roomKey) =>
      switch (StringMap.get(roomKey_gameState, roomKey)) {
      | None => ()
      | Some(gameForLeaving) =>
        switch (Game.maybeGetSocketPlayer(socket, gameForLeaving)) {
        | None => ()
        | Some(playerId) =>
          logger.debug2(
            {
              "player": {
                "username": Quad.select(playerId, player => player.Game.pla_name, gameForLeaving.players),
                "playerId": Player.stringOfId(playerId),
              },

              "game": Game.debugOfState(gameForLeaving),
            },
            "Removing player from game.",
          );
          My.Global.clearMaybeTimeout(gameForLeaving.maybeKickTimeoutId);
          let gameAftLeaving = GameReducer.reduce(LeaveGame(playerId), gameForLeaving)
          if (Game.playerCount(gameAftLeaving) == 0) {
            StringMap.remove(roomKey_gameState, roomKey);
          } else {
            StringMap.set(roomKey_gameState, roomKey, gameAftLeaving);
            updateClientStates(gameAftLeaving);

            if (gameAftLeaving->Game.isPublic && gameAftLeaving.phase->Game.isFindSubsPhase) {
              /* forall public games in FindPlayersPhase, inform the players that they can
                 choose to become a substitute player in an existing public game */
              StringMap.valuesToArray(roomKey_gameState)
              |> Belt.Array.keep(_, game => game->Game.isPublic && game.phase->Game.isFindPlayersPhase)
              |> Belt.Array.forEach( _, mutateAndPublish(GameReducer.UpdateSubbing(true), roomKey_gameState),
                 );
            };
          };

          SockServ.Socket.emit(socket, SocketMessages.Reset);
        }
      }
    }
  };
};

let kickPlayer = (player) => {
  switch(player.Game.pla_socket){
    | None => ()
    | Some(socket) => leaveGame(socket);
  }
};

let reconcileKickTimeout = (prevGameState, nextGameState) => {
  let prevMaybeActivePlayer = ActivePlayer.find(prevGameState.Game.phase, prevGameState.dealer);
  let nextMaybeActivePlayer = ActivePlayer.find(nextGameState.Game.phase, nextGameState.dealer);
  if (prevMaybeActivePlayer != nextMaybeActivePlayer) {
    switch (nextMaybeActivePlayer) {
    | None =>
      //clear the previous timeout
      My.Global.clearMaybeTimeout(prevGameState.maybeKickTimeoutId);
      nextGameState;
    | Some(nextActivePlayer) =>
      // clear the prev timeout
      My.Global.clearMaybeTimeout(prevGameState.maybeKickTimeoutId);
      // and setup a new one for the new active player
      let playerToKick = Quad.get(nextActivePlayer.id, nextGameState.players);

      let maybeKickTimeoutId =
        Some(
          Js.Global.setTimeout(
            () => kickPlayer(playerToKick),
            SharedGame.settings.kickPlayerMillis,
          ),
        );

      {...nextGameState, maybeKickTimeoutId};
    };
  } else {
    nextGameState;
  };
};

let joinGame = (socket, username, clientSettings) => {
  let socketId = SockServ.Socket.(socket->getId);
  let logger = logger.makeChild({"_context": "joinGame", "socketId": socketId});

  let isPublicGame = gameState =>
    switch (gameState.Game.game_id) {
    | Public(_) => true
    | _ => false
    };

  let publicGames = StringMap.valuesToArray(roomKey_gameState)
    |> Belt.Array.keep(_, Game.isPublic);
  let publicGamesFindingPlayers = Belt.Array.keep(publicGames, game => game.phase->Game.isFindPlayersPhase);
  let publicGamesFindingSubs  = Belt.Array.keep(publicGames, game => game.phase->Game.isFindSubsPhase);

  /** 
    Lag on the client or other issues might cause the player to try joining multiple
    times. When that happens just remind him which game he's in.
  */
  let maybeGetPlayerGame = (socket, gameStates) => {
    switch (
      Belt.Array.keep(gameStates, gameState =>
        Quad.exists(player => player.Game.pla_socket == Some(socket), gameState.Game.players)
      )
      |> Array.to_list
    ) {
    | [] => None
    | [gameState, ..._rest] => Some(gameState)
    };
  };
  
  switch(maybeGetPlayerGame(socket, publicGames)){
  | Some(gameState) => 
    let maybePlayerIdAndData = 
      Quad.getPairWhere(player => player.Game.pla_socket == Some(socket), gameState.Game.players);
    switch(maybePlayerIdAndData){
    | None => 
      logger.warn2(
        Game.debugOfState(gameState),
        "Found an existing game and expected Some player to have this socketId but got None.",
      );
    | Some((playerId, _)) =>
      let (playerId, playerPhase) = Game.decidePlayerPhase(gameState.phase, gameState.dealer, playerId)
      let clientState = buildClientState(gameState, playerId, playerPhase);
      updateClientState(socket, clientState)
    }
  | None => 

    let gameForJoining =
      switch (publicGamesFindingPlayers |> Array.to_list) {
      | [] => {
          ...Game.initialState(),
          game_id: Public(nextGameId() |> string_of_int),
          phase: FindPlayersPhase(3, Array.length(publicGamesFindingSubs) == 0 ? false : true),
        }
      | [gameState, ..._rest] => gameState
      };
    
    let roomKey = Game.stringOfGameId(gameForJoining.game_id);

    let socket = SockServ.Socket.join(socket, roomKey);
    StringMap.set(
      socketId_playerData,
      socketId,
      {maybeRoomKey: Some(roomKey)},
    );

    let playerId = Game.findEmptySeat(gameForJoining) |> Js.Option.getExn; // #unsafe #todo Handle attempt to join a full room or fix to ensure that unfilled room was actually unfilled
    let pla_name = username == "" ? Player.stringOfId(playerId) : username;
    let gameAftJoining = {
      ...gameForJoining,
      players:
        Quad.update(
          playerId,
          x => Game.{...x, pla_name, pla_socket: Some(socket)},
          gameForJoining.players,
        ),
    };

    let playerCount = Game.playerCount(gameAftJoining);
    let playersNeeded = 4 - playerCount;

    let phaseAftPhasing =
      switch (gameAftJoining.phase) {
      | FindSubsPhase(_n, subPhase) =>
        if (playersNeeded == 0) {
          subPhase;
        } else {
          FindSubsPhase(playersNeeded, subPhase);
        }

      | FindPlayersPhase(_n, canSub) =>
        playersNeeded == 0 ? DealPhase : FindPlayersPhase(playersNeeded, canSub)
      | otherPhase => otherPhase
      };

    if (gameAftJoining.phase->Game.isFindSubsPhase && not(phaseAftPhasing->Game.isFindSubsPhase)) {
      /* 
        This may be the last game to exit the FindSubPhase Any games that are in the
        FindPlayersPhase needs to disable the option for subbing.
      */
      let canSub =
        publicGames |> Belt.Array.some(_, gameState => Game.isFindSubsPhase(gameState.phase));

      if (!canSub) {
        Belt.Array.keep(publicGames, gameState => Game.isFindPlayersPhase(gameState.phase))
        |> Belt.Array.forEach(
             _,
             mutateAndPublish(GameReducer.UpdateSubbing(false), roomKey_gameState),
           );
      };
    };

    let playerJoinedNotis =
      Noti.playerBroadcast(
        ~from=playerId,
        ~msg=Noti.Text(pla_name ++ " joined the game."),
        ~level=Noti.Success,
        (),
      );

    let gameAftUpdating = {
      ...gameAftJoining,
      phase: phaseAftPhasing,
      notis: gameAftJoining.notis @ playerJoinedNotis,
    };

    let gameAftKickReconciling = reconcileKickTimeout(gameForJoining, gameAftUpdating);

    logger.info2(Game.debugOfState(gameAftKickReconciling), "Saving game state." );

    StringMap.set(roomKey_gameState, roomKey, gameAftKickReconciling);
    updateClientStates(gameAftKickReconciling);
  }
};

let rec makePrivateGame = (socket, username, clientSettings) => {
  /* create a random ID made from the string of 4 cards picked from the deck */
  let strId =
    Deck.make()
    |> Deck.shuffle
    |> Belt.List.take(_, 4)
    |> Js.Option.getExn
    |> List.map(Card.codeOfCard)
    |> Belt.List.toArray
    |> Js.Array.joinWith(" ");

  Js.log2("gameId = ", strId);

  /* Ensure this id is unique in the list of all games */
  /* If the id is a duplicate */
  let gameIdExists = (searchId, gameState) =>
    switch (gameState.Game.game_id) {
    | Public(_) => false
    | Private(id) => searchId == id
    };

  let gameStates = StringMap.valuesToArray(roomKey_gameState);
  if (Belt.Array.some(gameStates, gameIdExists(strId))) {
    makePrivateGame(socket, username, clientSettings);
  } else {
    Game.{...initialState(), game_id: Private(strId)};
  };
};

let joinGame2 = (socket, username, gameState) => {
  let socketId = SockServ.Socket.getId(socket);
  let str_game_id = Game.(stringOfGameId(gameState.game_id));
  switch (Game.findEmptySeat(gameState)) {
  | None => Belt.Result.Error("The game is already full.")
  | Some(playerId) =>
    let pla_name = username == "" ? Player.stringOfId(playerId) : username;
    let socket = SockServ.Socket.join(socket, str_game_id);
    StringMap.set(socketId_playerData, socketId, {maybeRoomKey: Some(str_game_id)});

    let players =
      Quad.update(
        playerId,
        player => Game.{...player, pla_name, pla_socket: Some(socket)},
        gameState.players,
      );

    let playersNeeded = 4 - Game.countPlayers(players);
    let phase =
      switch (gameState.phase) {
      | FindSubsPhase(_n, subPhase) =>
        playersNeeded == 0 ? subPhase : FindSubsPhase(playersNeeded, subPhase)
      | FindPlayersPhase(_n, false) =>
        playersNeeded == 0 ? DealPhase : FindPlayersPhase(playersNeeded, false)
      | otherPhase => otherPhase
      };

    Belt.Result.Ok({...gameState, players, phase});
  };
};


let onSocketDisconnect = socket =>
  SockServ.Socket.onDisconnect(
    socket,
    () => {
      /** Note: by this time socketio has already removed the socket from the room.
        Also, during testing I noticed that refreshing the app in chrome rapidly multiple times
        sometimes results in socketio not immediately detecting a disconnect. However it 
        seems to triage after about a minute(?) so there is a delay between when you expect to 
        see a disconnect and when socketio finally fires this disconnect handler.
        
        The result is that there may be open game rooms holding sockets that are effectively dead.*/

      let socketId = SockServ.Socket.getId(socket);
      let logger = logger.makeChild({"_context": "socket-onDisconnect", "socketId": socketId});

      logger.info("Socket disconnected.");
      leaveGame(socket);
      StringMap.remove(socketId_playerData, socket->SockServ.Socket.getId);
      logger.info2(getGameStats(), "Game stats:");
    },
  );

SockServ.onConnect(
  io,
  socket => {
    let socketId = SockServ.Socket.getId(socket);
    let logger = logger.makeChild({"_context": "socket-onconnect", "socketId": socketId});

    logger.info("Socket connected");
    onSocketDisconnect(socket);
    SockServ.Socket.emit(socket, SocketMessages.Reset);

    SockServ.Socket.on(socket, io => {
      let stringOfEvent = SocketMessages.stringOfClientToServer(io);
      let logger = logger.makeChild({"_context": "socket-onevent", "event": stringOfEvent});
      logger.debug2("Handling `%s` event. ", stringOfEvent);
      switch (io) {
      | IO_StartPrivateGame(username, ioClientSettingsJson) =>
        let clientSettings =
          decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
        let gameState = makePrivateGame(socket, username, clientSettings);
        let str_game_id = Game.(stringOfGameId(gameState.game_id));
        switch (joinGame2(socket, username, gameState)) {
        | Belt.Result.Error(e) => logger.error2({"error": e}, "Failed to join game")
        | Belt.Result.Ok(gameState) =>
          logger.info2(Game.debugOfState(gameState), "Saving game state and updating clients.");

          StringMap.set(roomKey_gameState, str_game_id, gameState);
          updateClientStates(gameState);
          logger.info2(getGameStats(), "Game stats:");
        };
      | IO_JoinPrivateGame(_inviteCode, _username, _ioClientSettings) => ()
      | IO_JoinGame(username, ioClientSettingsJson) =>
        let clientSettings =
          decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);

        joinGame(socket, username, clientSettings);
        logger.info2(getGameStats(), "Game stats:");
      | IO_LeaveGame => 
        leaveGame(socket);
        logger.info2(getGameStats(), "Game stats:");
      | IO_PlayAgain(username, ioClientSettingsJson) =>
        leaveGame(socket);
        let clientSettings =
          decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
        joinGame(socket, username, clientSettings);
        logger.info2(getGameStats(), "Game stats:");
      | ioAction =>
        let roomKey =
          switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
          | None => ""
          | Some({maybeRoomKey}) => Js.Option.getWithDefault("", maybeRoomKey)
          };
        switch (StringMap.get(roomKey_gameState, roomKey)) {
        | None => ()
        | Some(prevGameState) =>
          let roomKey = Game.stringOfGameId(prevGameState.game_id);
          let action = ioAction |> actionOfIO_Action;
          
          let nextGameState = GameReducer.reduce(action, prevGameState);

          let nextGameState = reconcileKickTimeout(prevGameState, nextGameState);

          let isEndTrick =
            Quad.map(
              player => Js.Option.isSome(player.Game.pla_card) ? true : false,
              nextGameState.players,
            )
            |> Quad.foldLeftUntil(
                 (iPlayed, wePlayed) => wePlayed && iPlayed,
                 wePlayed => !wePlayed,
                 true,
               );

          if (isEndTrick) {
            let advanceRound = () => {
              switch (StringMap.get(roomKey_gameState, roomKey)) {
              | None => ()
              | Some(prevGameState) =>
                let nextGameState = GameReducer.reduce(AdvanceRound, prevGameState);
                let nextGameState = reconcileKickTimeout(prevGameState, nextGameState);
                StringMap.set(roomKey_gameState, roomKey, nextGameState);
                updateClientStates(nextGameState);
              };
            };

            Js.Global.setTimeout(advanceRound, 2750) |> ignore;
          };

          logger.info2(Game.debugOfState(nextGameState), "Saving game state" );
          StringMap.set(roomKey_gameState, roomKey, nextGameState);
          updateClientStates(nextGameState);
        };
      }
    });
    SockServ.Socket.onWithAck(
      socket,
      (io, ack) => {
        let stringOfEvent = SocketMessages.stringOfClientToServer(io);
        let logger =
          logger.makeChild({"_context": "socket-onevent-with-ack", "event": stringOfEvent});
        logger.debug2("Handling `%s` event. ", stringOfEvent);
        switch (io) {
        | IO_JoinPrivateGame(inviteCode, username, ioClientSettings) =>
          logger.info("Invite code: " ++ inviteCode);
          /** Find game having the provided invite code */
          let gameMatchesCode = (inviteCode, gameState) => {
            let normalizeCode = code =>
              /** Lowercases all letters and removes spaces */ Js.String.toLowerCase(code)
              |> Js.String.replaceByRe([%re "/ /g"], "");
            switch (gameState.Game.game_id) {
            | Public(_) => false
            | Private(code) => normalizeCode(code) == normalizeCode(inviteCode)
            };
          };

          let matchingGames = StringMap.valuesToArray(roomKey_gameState)
          |> Belt.Array.keep(_, gameMatchesCode(inviteCode) )
          |> Belt.List.fromArray;

          let maybeGameState =
            switch (matchingGames) {
            | [gameState, ..._rest] => Some(gameState)
            | _ => None
            };

          let result =
            switch (maybeGameState) {
            | None => Belt.Result.Error("Game not found.")
            | Some(gameState) =>
              switch (joinGame2(socket, username, gameState)) {
              | Belt.Result.Ok(gameState2) => Belt.Result.Ok((gameState, gameState2))
              | _ => Belt.Result.Error("Failed to join game")
              }
            };
            

          switch(result){
          | Belt.Result.Error(message) => 
            ack(SocketMessages.AckError(message))
          | Belt.Result.Ok((gameState, gameState2)) => 
            switch(gameState2.game_id){
            | Public(_) => 
              ack(SocketMessages.AckError("Game not found."))
            | Private(code) =>
              let gameStateAftReconcileKick = reconcileKickTimeout(gameState, gameState2);
              StringMap.set(roomKey_gameState, code, gameStateAftReconcileKick);
              updateClientStates(gameStateAftReconcileKick); 
              ack(SocketMessages.AckOk)
            }
          };

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
      let gameStates = StringMap.valuesToArray(roomKey_gameState);
      <div>
        <div>
          <span> {"Games since server started: " |> ReasonReact.string} </span>
          <span> {nGamesCreated^ |> string_of_int |> ReasonReact.string} </span>
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
