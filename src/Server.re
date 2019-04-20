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

module Rooms = {
  type t = BsSocketExtra.AdapterRoom.t;
  let compare = compare;
};

module RoomsMap = Map.Make(Rooms);

// let gameRoomKeys = ref(RoomsMap.empty: RoomsMap.t(string));

let getKeysToRooms = () =>
  Namespace.getAdapter(ns) |> Obj.magic |> BsSocketExtra.Adapter.rooms;

type gameRoomPhase =
  | Waiting
  | Started;

type gameRoom = {
  key: string,
  room: BsSocketExtra.AdapterRoom.t,
  phase: gameRoomPhase,
};

// let stringOfGameRoom = gameRoom => {
//   let socketIds =
//     Js.Dict.keys(BsSocketExtra.AdapterRoom.sockets(gameRoom.room));
//   {j|{ key: $gameRoom.key, sockets: $socketIds |j};
// };

// let stringOfGameRooms =

let gameRooms = ref([]: list(gameRoom));

let playerCount = gameRoom => BsSocketExtra.AdapterRoom.length(gameRoom.room);
let isRoomFull = gameRoom => playerCount(gameRoom) == 4;

let unfilledRoom = gameRooms => {
  let unfilledRooms =
    List.filter(gameRoom => playerCount(gameRoom) < 4, gameRooms);
  switch (unfilledRooms) {
  | [] => None
  | [r, ..._rs] => Some(r)
  };
};

SockServ.onConnect(
  io,
  socket => {
    print_endline("Got a connection!");
    // let rooms = Js.Dict.values(keyToRooms);
    // Js.log2("Rooms this socket is in: ", rooms);
    // print_endline(Namespace.clients());

    let gameRoom =
      switch (unfilledRoom(gameRooms^)) {
      | None =>
        let roomKey = nanoid();
        let _socket = SockServ.Socket.join(socket, roomKey);
        let keysToRooms = getKeysToRooms();
        let room = Js.Dict.unsafeGet(keysToRooms, roomKey);
        let gameRoom = {key: roomKey, room, phase: Waiting};
        gameRooms := [gameRoom, ...gameRooms^];
        gameRoom;
      | Some(room) =>
        SockServ.Socket.join(socket, room.key) |> ignore;
        room;
      };

    if (gameRoom.phase == Waiting && isRoomFull(gameRoom)) {
      Js.log2("Sending start to ", gameRoom.key);
      ns->Namespace.to_(gameRoom.key)->Namespace.emit(Start);
    };

    Js.log2("gameRooms length", List.length(gameRooms^));
    Js.log2("Game rooms: ", gameRooms);
    // let x = SockServ.Socket.getId(socket);
    // Js.log2("Rooms this socket is in: ", rooms);
    /* Polymorphic pipe which actually knows about ExampleCommon.t from InnerServer */
    SockServ.Socket.on(
      socket,
      fun
      | SocketMessages.Deal => {
          Js.log("got deal");
        }
      | NewRound => {
          Js.log("Got NewRound message");
          SockServ.Socket.emit(socket, Ok);
        },
    );
  },
);

Http.listen(http, 3000, () => print_endline("listening on *:3000"));
