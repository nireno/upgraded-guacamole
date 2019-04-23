[%%debugger.chrome];
[@bs.val] external node_env: string = "process.env.NODE_ENV";
[@bs.module]
external webpackConfig: Webpack.configuration = "../webpack.config.js";

[@bs.module] external nanoid: unit => string = "";

let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log @@ "Listening at http://127.0.0.1:3000"
  };

let app = Express.express();

let webpackMiddleware =
  Webpack.(webpackConfig |> webpack |> webpackDevMiddleware);
Express.App.use(app, webpackMiddleware);

Express.App.useOnPath(
  app,
  ~path="/",
  Express.Static.(make("/build", defaultOptions()) |> asMiddleware),
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

let debugGameRooms = () => {
  let roomCount = StringMap.size(gameRooms);
  let strAre = Grammer.byNumber(roomCount, "is");
  let strRooms = Grammer.byNumber(roomCount, "room");
  let strRoomCount = string_of_int(roomCount);
  Js.log({j|There $strAre $strRoomCount $strRooms.|j});
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

let getClientState: (Player.id, Game.state) => ClientGame.state =
  (player, state) => {
    ClientGame.{
      phase: state.phase,
      me: player,
      dealer: state.dealer,
      leader: state.leader,
      maybePlayerTurn: state.maybePlayerTurn,
    };
  };

let onSocketDisconnect = socket =>
  SockServ.Socket.onDisconnect(
    socket,
    () => {
      Js.log2("Socket disconnected: ", SockServ.Socket.getId(socket));
      /** I wanted to drop all rooms where the disconnected socket is its only tenant.
          But apparently by this time socketio has already removed the socket from the room.
          So I just need to keep all non-empty rooms.
        */
      // let dropEmptyGame: Game.state => unit = ({Game.room, roomKey}) =>
      //   if(!BsSocketExtra.AdapterRoom.isEmpty(room) ){
      //   StringMap.remove(gameRooms, roomKey);
      //   }
      StringMap.toArray(gameRooms)
      |> Js.Array.filter(((_key, gr)) =>
           BsSocketExtra.AdapterRoom.isEmpty(gr.Game.room)
         )
      |> Js.Array.forEach(((key, _gr)) => StringMap.remove(gameRooms, key));

      debugGameRooms();
    },
  );

let updateClientStates = gameRoom => {
  Game.getAllPlayerSockets(gameRoom)
  |> List.iter(((player, socket)) => {
       let clientState = getClientState(player, gameRoom);
       let msg: SocketMessages.serverToClient =
         SetState(
           clientState
           |> ClientGame.stateToJs
           |> Obj.magic                                       // #unsafe
           |> Js.Json.stringify,
         );
       SocketMessages.debugServerMsg(msg, ());
       SockServ.Socket.emit(socket, msg); 
     });
};

SockServ.onConnect(
  io,
  socket => {
    let socketId = SockServ.Socket.getId(socket);
    print_endline("Socket connected: " ++ socketId);

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
    let gameRoom = {...gameRoom, phase: FindPlayersPhase(playersNeeded)};

    Js.log("Players needed: " ++ string_of_int(playersNeeded));

    StringMap.set(gameRooms, gameRoom.roomKey, gameRoom);

    updateClientStates(gameRoom);

    if (gameRoom.phase == FindPlayersPhase(0)) {
      SocketMessages.debugServerMsg(Start, ());
      ns->Namespace.to_(gameRoom.roomKey)->Namespace.emit(Start);
    };

    debugGameRooms();
    SockServ.Socket.on(
      socket,
      fun
      | Ping => {
          Js.log("Got Ping");
        }
      | SocketMessages.Deal => {
          Js.log("got deal");
        },
    );
    onSocketDisconnect(socket);
  },
);

Http.listen(http, 3000, () => print_endline("listening on *:3000"));
