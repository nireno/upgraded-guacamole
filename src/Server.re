[%%debugger.chrome];
open AppPrelude;

[@bs.val] external node_env: string = "process.env.NODE_ENV";

[@bs.module] external nanoid: unit => string = "";

let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log @@ "Listening at http://127.0.0.1:3000"
  };

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
let gameRooms: StringMap.t(Game.state) =
  StringMap.make(~hintSize=expectedGameCount);

let debugGameRooms = (~n=0, ()) => {
  let roomCount = StringMap.size(gameRooms);
  let strAre = Grammar.byNumber(roomCount, "is");
  let strRooms = Grammar.byNumber(roomCount, "room");
  let strRoomCount = string_of_int(roomCount);
  Js.log({j|There $strAre $strRoomCount $strRooms.|j} |> leftPad(_, ~n=n, ()));
};

let unfilledRoom: StringMap.t(Game.state) => option(Game.state) =
  gameRooms => {
    let gameRooms = StringMap.valuesToArray(gameRooms) |> Array.to_list;
    let unfilledRooms =
      List.filter(gameRoom => Game.playerCount(gameRoom) < 4, gameRooms);
    switch (unfilledRooms) {
    | [] => None
    | [r, ..._rs] => Some(r)
    };
  };


let buildClientState = (activePlayer, activePlayerPhase, gameState, player, playerPhase) => {
  let playerHand = Game.getPlayerHand(player, gameState);
  let hand =
    if (SharedGame.isFaceDownPhase(gameState.phase)) {
      player == gameState.dealer || player == gameState.leader 
        ? ClientGame.FaceUpHand(playerHand) 
        : ClientGame.FaceDownHand(List.length(playerHand));
    } else {
      ClientGame.FaceUpHand(playerHand);
    };

  ClientGame.{
    gameId: gameState.roomKey,
    phase: playerPhase,
    gamePhase: gameState.phase,
    me: player,
    dealer: gameState.dealer,
    leader: gameState.leader,
    activePlayer,
    activePlayerPhase,
    maybePlayerTurn: gameState.maybePlayerTurn,
    hand,
    maybeTrumpCard: gameState.maybeTrumpCard,
    maybeLeadCard: gameState.maybeLeadCard,
    board: gameState.board,
    team1Points: gameState.team1Points,
    team2Points: gameState.team2Points,
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
    (gameState|>Game.getPlayerSocket(player), buildClientState(player, playerPhase)))
}

let updateClientStates = gameRoom =>
  gameRoom
  ->buildSocketStatePairs
  ->Belt.List.forEach(((socket, clientState)) =>
      switch (socket) {
      | None => ()
      | Some(socket) =>
        // clientState->ClientGame.debugState(~ctx="Server.updateClientStates", ())
        let msg: SocketMessages.serverToClient =
          SetState(
            clientState |> SocketMessages.jsonOfClientGameState // #unsafe
          );
        SockServ.Socket.emit(socket, msg);
      }
    );

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

      StringMap.toArray(gameRooms)
      |> Belt.Array.forEach(_, ((key, game)) =>
           switch (Game.maybeGetSocketPlayer(socket, game)) {
           | None => ()
           | Some(player) =>
             let game = Game.removePlayerSocket(player, game);
             if (Game.isEmpty(game)) {
               StringMap.remove(gameRooms, key);
             } else {
               let game = 
               switch(game.phase){
                 | FindPlayersPhase(_n) => {...game, phase: FindPlayersPhase(4 - Game.playerCount(game))}
                 | _ => {...game, phase: FindSubsPhase(4 - Game.playerCount(game), game.phase)}
               };
               StringMap.set(gameRooms, key, game);
               updateClientStates(game);
             };
           }
         );

      debugGameRooms(~n=1, ());
    },
  );


let actionOfIO_Action: SocketMessages.clientToServer => Game.action =
  fun
  | IO_PlayCard(ioPlayerId, cardStr_json) => {
      let card = SocketMessages.cardOfJsonUnsafe(cardStr_json);
      switch (SocketMessages.maybePlayerOfIO(ioPlayerId)) {
      | None => Noop
      | Some(player) => PlayCard(player, card)
      };
    }
  | IO_BlockPlay(ioPlayerId) => {
      switch (SocketMessages.maybePlayerOfIO(ioPlayerId)) {
      | None => Noop
      | Some(player) => BlockPlay(player)
      };
    }
  | IO_EndTrick => EndTrick
  | IO_NewRound => NewRound
  | IO_EndRound => EndRound
  | IO_Beg => Beg
  | IO_Stand => Stand
  | IO_GiveOne => GiveOne
  | IO_Deal => Deal
  | IO_RunPack => RunPack
  | IO_DealAgain => DealAgain
  | IO_CheatPoints(ioTeamId, points) =>
    switch (SocketMessages.maybeTeamOfIO(ioTeamId)) {
    | None => Noop
    | Some(team) => CheatPoints(team, points)
    };


SockServ.onConnect(
  io,
  socket => {
    onSocketDisconnect(socket);

    let socketId = SockServ.Socket.(socket->getId);
    Js.log("Server: onConnect");
    debugSocket(socket, ~n=1, ());

    /** Consider changing names with "room" to game where room is actually a Game.state */
    let maybeUnfilledRoom = gameRoom =>
      switch (gameRoom |> Game.findEmptySeat) {
      | None => None
      | Some(_) => Some(gameRoom)
      };

    let unfilledRooms =
      gameRooms
      |> StringMap.valuesToArray
      |> Belt.Array.keepMap(_, gameRoom => maybeUnfilledRoom(gameRoom))
      |> Array.to_list;

    let gameRoom =
      switch (unfilledRooms) {
      | [] =>
        let keysToRooms = getKeysToRooms();
        let room = Js.Dict.unsafeGet(keysToRooms, socketId);
        {
          ...Game.initialState(),
          roomKey: socketId,
          room,
        };
      | [gameRoom, ..._rest] => gameRoom
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

    let _socket = SockServ.Socket.join(socket, gameRoom.roomKey);
    let playerId = Game.findEmptySeat(gameRoom) |> Js.Option.getExn; // #unsafe #todo Handle attempt to join a full room or fix to ensure that unfilled room was actually unfilled

    let gameRoom = Game.updatePlayerSocket(playerId, socket, gameRoom);
    let playerCount = Game.playerCount(gameRoom);
    let playersNeeded = 4 - playerCount;

    let gameRoom =
        switch(gameRoom.phase){
          | FindSubsPhase(_n, subPhase) => 
            playersNeeded == 0 ? {...gameRoom, phase: subPhase} : {...gameRoom, phase: FindSubsPhase(playersNeeded, subPhase)}
          | FindPlayersPhase(_n) => 
            playersNeeded == 0 ? {...gameRoom, phase: DealPhase} : {...gameRoom, phase: FindPlayersPhase(playersNeeded)}
          | _ => gameRoom
        }

    StringMap.set(gameRooms, gameRoom.roomKey, gameRoom);

    updateClientStates(gameRoom);

    debugGameRooms(~n=1, ());

    SockServ.Socket.on(socket, ioAction =>
      switch (StringMap.get(gameRooms, gameRoom.roomKey)) {
      | None => ()
      | Some(gameRoom) =>
        let gameRoom = GameReducer.reducer(ioAction |> actionOfIO_Action, gameRoom);
        StringMap.set(gameRooms, gameRoom.roomKey, gameRoom);
        updateClientStates(gameRoom);
      }
    );

  },
);

Http.listen(http, 3000, () => print_endline("listening on *:3000"));
