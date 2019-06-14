[%%debugger.chrome];
open AppPrelude;

[@bs.val] external nodeEnv: string = "process.env.NODE_ENV";
[@bs.val] external httpPortEnv: Js.Nullable.t(string) = "process.env.ALL_FOURS_PORT";

[@bs.module] external nanoid: unit => string = "";


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

let io = SockServ.createWithHttp(http);
let ns = Namespace.of_(io, "/");

let getKeysToRooms = () =>
  Namespace.getAdapter(ns) |> Obj.magic |> BsSocketExtra.Adapter.rooms;

let expectedGameCount = 50;
module StringMap = Belt.HashMap.String;

type playerData = {
  username: string,
  maybeRoomKey: option(string),
};

/** Map of player sockets to their respective game data */
let socketId_playerData: StringMap.t(playerData) = 
  StringMap.make(~hintSize=(expectedGameCount*4));

/** Map of room keys to respective game states */
let roomKey_gameState: StringMap.t(Game.state) =
  StringMap.make(~hintSize=expectedGameCount);

let debugGameStates = (~n=0, ()) => {
  let roomCount = StringMap.size(roomKey_gameState);
  let strAre = Grammar.byNumber(roomCount, "is");
  let strRooms = Grammar.byNumber(roomCount, "room");
  let strRoomCount = string_of_int(roomCount);
  Js.log({j|There $strAre $strRoomCount $strRooms.|j} |> leftPad(_, ~n=n, ()));
};

let debugGameBySocket = socket => {
  switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
  | None => ()
  | Some({maybeRoomKey}) =>
    switch (StringMap.get(roomKey_gameState, maybeRoomKey |> Js.Option.getWithDefault(""))) {
    | None => ()
    | Some(gameState) => Js.log(Game.stringOfState(gameState))
    }
  };
};

let buildClientState = (activePlayer, activePlayerPhase, gameState, player, playerPhase) => {
  let playerHand = GamePlayers.get(player, gameState.Game.players).pla_hand;
  let handFacing =
    if (SharedGame.isFaceDownPhase(gameState.phase)) {
      player == gameState.dealer || player == gameState.leader 
        ? Hand.FaceUpHand(playerHand) 
        : Hand.FaceDownHand(List.length(playerHand));
    } else {
      Hand.FaceUpHand(playerHand);
    };

  let p1State = GamePlayers.get(P1, gameState.players);
  let p2State = GamePlayers.get(P2, gameState.players);
  let p3State = GamePlayers.get(P3, gameState.players);
  let p4State = GamePlayers.get(P4, gameState.players);
  ClientGame.{
    gameId: gameState.roomKey,
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
    myTricks: GamePlayers.get(player, gameState.players).pla_tricks,
    dealer: gameState.dealer,
    leader: gameState.leader,
    activePlayer,
    activePlayerPhase,
    maybePlayerTurn: gameState.maybePlayerTurn,
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
      gameState.dealer,
      gameState.leader,
      gameState.maybePlayerTurn)

  let playerPhasePairs: list((Player.id, Player.phase)) = 
    [Player.P1, P2, P3, P4] 
      -> Belt.List.map(p => p->decidePlayerPhase)

  let (activePlayer, activePlayerPhase) =
    switch(playerPhasePairs->Belt.List.keep(((_player, playerPhase))=> playerPhase != Player.PlayerIdlePhase)){
      | [pp] => pp
      | _ => (P1, PlayerIdlePhase)
    }

  let buildClientState = buildClientState(activePlayer, activePlayerPhase, gameState);
  playerPhasePairs->Belt.List.map(
    ( (player, playerPhase) ) => 
    (GamePlayers.select(player, x => Game.(x.pla_socket), gameState.players), buildClientState(player, playerPhase)))
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
  let gameState' = GameReducer.reducer(ClearNotis, gameState);

  StringMap.set(roomKey_gameState, gameState'.roomKey, gameState');
};

let debugSocket: (BsSocket.Server.socketT, ~ctx: string=?, ~n: int=?, unit) => unit = 
  (socket, ~ctx="", ~n=0, ()) => {
  let socketToString: BsSocket.Server.socketT => string = 
    [%raw (socket) => {j|
    return "socketT.{" + "\n" +
        "\t" + "id: " + socket.id + "\n" +
        "\t" + "connected: " + socket.connected + "\n" +
        "\t" + "disconnected: " + socket.disconnected + "\n" + 
      "}"|j}];

  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(socket |> socketToString |> leftPad(_, ~n, ()));
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

      Js.log("Server:onSocketDisconnect");
      debugSocket(socket, ~n=1, ());

      StringMap.toArray(roomKey_gameState)
      |> Belt.Array.forEach(_, ((key, game)) =>
           switch (Game.maybeGetSocketPlayer(socket, game)) {
           | None => ()
           | Some(player) =>
             let game = GameReducer.reducer(LeaveGame(player), game);
             if (Game.isEmpty(game)) {
               StringMap.remove(roomKey_gameState, key);
             } else {
               StringMap.set(roomKey_gameState, key, game);
               updateClientStates(game);
             };
           }
         );

      debugGameBySocket(socket);

      StringMap.remove(socketId_playerData, socket->SockServ.Socket.getId);
      debugGameStates(~n=1, ());
    },
  );


let actionOfIO_Action: SocketMessages.clientToServer => Game.action =
  fun
  | IO_JoinGame(_)
  | IO_LeaveGame
  | IO_PlayAgain => Noop
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

let joinGame = (socket, username) => {
  let socketId = SockServ.Socket.(socket->getId);

  /** An unfilled game is one that is new/ongoing and has at least one player missing.
      A game in the GameOverPhase is not considered to be ongoing. */
  let maybeUnfilledGame = gameState =>
    switch (gameState |> Game.findEmptySeat) {
    | None => None
    | Some(_) when gameState.phase != GameOverPhase => Some(gameState)
    | _ => None
    };

  let unfilledGames =
    roomKey_gameState
    |> StringMap.valuesToArray
    |> Belt.Array.keepMap(_, gameState => maybeUnfilledGame(gameState))
    |> Array.to_list;

  let gameState =
    switch (unfilledGames) {
    | [] => {...Game.initialState(), roomKey: socketId}
    | [gameState, ..._rest] => gameState
    };

  // let maybeRoom =
  //   ns
  //   |> Namespace.getAdapter
  //   |> BsSocketExtra.Adapter.getRoom(gameRoom.roomKey);
  // switch (maybeRoom) {
  // | Some(ar) =>
  // let playerCount = BsSocketExtra.AdapterRoom.length(ar);
  // | None => failwith("Expected Some AdapterRoom but got None.") // Improve error handling #unsafe #todo
  // };

  let socket = SockServ.Socket.join(socket, gameState.roomKey);
  StringMap.set(
    socketId_playerData,
    socketId,
    {username, maybeRoomKey: Some(gameState.roomKey)},
  );

  let playerId = Game.findEmptySeat(gameState) |> Js.Option.getExn; // #unsafe #todo Handle attempt to join a full room or fix to ensure that unfilled room was actually unfilled
  let pla_name = username == "" ? Player.stringOfId(playerId) : username;
  let gameState =
    {
      ...gameState,
      players:
        GamePlayers.update(
          playerId,
          x =>
            Game.{
              ...x,
              pla_name: pla_name,
              pla_socket: Some(socket),
            },
          gameState.players,
        ),
    };

  let playerCount = Game.playerCount(gameState);
  let playersNeeded = 4 - playerCount;

  let phase' = 
    switch (gameState.phase) {
    | FindSubsPhase(_n, subPhase) =>
      playersNeeded == 0
        ? subPhase
        : FindSubsPhase(playersNeeded, subPhase)
    | FindPlayersPhase(_n) =>
      playersNeeded == 0
        ? DealPhase
        : FindPlayersPhase(playersNeeded);
    | otherPhase => otherPhase
  };

  let playerJoinedNotis =
    Noti.playerBroadcast(
      ~from=playerId,
      ~msg=Noti.Text(pla_name ++ " joined the game."),
      ~level=Noti.Success,
      (),
    );

  let gameState = {
    ...gameState,
    phase: phase',
    notis: gameState.notis @ playerJoinedNotis,
  };

  Js.log(Game.stringOfState(gameState));
  StringMap.set(roomKey_gameState, gameState.roomKey, gameState);
  updateClientStates(gameState);
};

let leaveGame = socket => {
  switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
  | None => ()
  | Some({maybeRoomKey}) =>
    switch (maybeRoomKey) {
    | None => ()
    | Some(roomKey) =>
      switch (StringMap.get(roomKey_gameState, roomKey)) {
      | None => ()
      | Some(gameState) =>
        switch (Game.maybeGetSocketPlayer(socket, gameState)) {
        | None => ()
        | Some(playerId) =>
          Js.log(Player.stringOfId(playerId) ++ " is leaving the game");
          let gameState' = GameReducer.reducer(LeaveGame(playerId), gameState);
          if (Game.playerCount(gameState') == 0) {
            StringMap.remove(roomKey_gameState, roomKey);
          } else {
            StringMap.set(roomKey_gameState, roomKey, gameState');
          };
          updateClientStates(gameState');
          
          SockServ.Socket.emit(socket, SocketMessages.Reset);
        }
      }
    }
  };
};

SockServ.onConnect(
  io,
  socket => {
    Js.log("Server: onConnect");
    debugSocket(socket, ~n=1, ());

    onSocketDisconnect(socket);
    SockServ.Socket.emit(socket, SocketMessages.Reset);

    SockServ.Socket.on(socket, io =>
      switch (io) {
      | IO_JoinGame(username) =>
        joinGame(socket, username);

        debugGameStates(~n=1, ());
      
      | IO_LeaveGame => 
        Js.log("Got IO_LeaveGame");
        leaveGame(socket);
      | IO_PlayAgain =>
        Js.log("Got IO_PlayAgain");
        switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
        | None => ()
        | Some({username, maybeRoomKey: _}) =>
            leaveGame(socket);
            joinGame(socket, username);
        };
      | ioAction =>
        let roomKey =
          switch (StringMap.get(socketId_playerData, socket->SockServ.Socket.getId)) {
          | None => ""
          | Some({maybeRoomKey}) => Js.Option.getWithDefault("", maybeRoomKey)
          };
        switch (StringMap.get(roomKey_gameState, roomKey)) {
        | None => ()
        | Some(gameState) =>
          let action = ioAction |> actionOfIO_Action;

          let isEndTrick =
            switch (action) {
            | PlayCard(player, _) => Player.nextPlayer(player) == gameState.leader
            | _ => false
            };

          let gameState = GameReducer.reducer(action, gameState);

          let advanceRound = () => {
            let gameState = GameReducer.reducer(AdvanceRound, gameState);
            StringMap.set(roomKey_gameState, gameState.roomKey, gameState);
            updateClientStates(gameState);
          };

          if (isEndTrick) {
            Js.Global.setTimeout(advanceRound, 2750) |> ignore;
          };

          StringMap.set(roomKey_gameState, gameState.roomKey, gameState);
          updateClientStates(gameState);
        };
      }
    );
  },
);


let httpPort = Js.Nullable.toOption(httpPortEnv)
 |> Js.Option.getWithDefault("3000")
 |> int_of_string;

Http.listen(http, httpPort, () => print_endline("Listening on *:" ++ string_of_int(httpPort)));
