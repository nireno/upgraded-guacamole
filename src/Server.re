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
  | IO_DealAgain => DealAgain;

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
      ServerStore.dispatch(RemovePlayerBySocket(socketId));
      SocketServer.Store.dispatch(RemoveSocket(socket));
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

    SocketServer.Socket.on(
      socket,
      io => {
        let stringOfEvent = SocketMessages.stringOfClientToServer(io);
        let logger = logger.makeChild({"_context": "socket-onevent", "event": stringOfEvent});
        logger.debug2("Handling `%s` event. ", stringOfEvent);
        switch (io) {
        | IO_StartPrivateGame(username, _ioClientSettingsJson) =>
          ServerStore.dispatch(CreatePrivateGame(sock_id, username));
          logger.info2(getGameStats(), "Game stats:");

        | IO_JoinPrivateGame(_inviteCode, _username, _ioClientSettings) => () // handled later by Socket.onWithAck

        | IO_JoinGame(username, _ioClientSettingsJson) =>
          ServerStore.dispatch(AttachPublicPlayer(sock_id, username));
          logger.info2(getGameStats(), "Game stats:");

        | IO_LeaveGame =>
          ServerStore.dispatch(RemovePlayerBySocket(sock_id));
          logger.info2(getGameStats(), "Game stats:");

        | IO_PlayAgain(username, _ioClientSettingsJson) =>
          switch (ServerStore.getGameBySocket(sock_id)) {
          | None => ServerStore.dispatch(TriggerEffects([ServerEffect.ResetClient(sock_id)]))
          | Some(gameState) =>
            switch (gameState.game_id) {
            | Public(_) =>
              ServerStore.dispatchMany([
                RemovePlayerBySocket(sock_id),
                AttachPublicPlayer(sock_id, username),
              ])
            | Private(_) =>
              ServerStore.dispatchMany([
                RemovePlayerBySocket(sock_id),
                CreatePrivateGame(sock_id, username)]);
            }
          };
          logger.info2(getGameStats(), "Game stats:");

        | IO_Substitute(username) => ServerStore.dispatch(AttachSubstitute(sock_id, username))

        | ioAction =>
          let action = ioAction |> actionOfIO_Action;
          ServerStore.dispatch(UpdateGameBySocket(sock_id, action));
        };
      },
    );

    SocketServer.Socket.onWithAck(
      socket,
      (io, ack) => {
        let stringOfEvent = SocketMessages.stringOfClientToServer(io);
        let logger =
          logger.makeChild({"_context": "socket-onevent-with-ack", "event": stringOfEvent});

        // logger.debug2("Handling `%s` event. ", stringOfEvent);

        switch (io) {
        | IO_JoinPrivateGame(inviteCode, username, _ioClientSettings) =>
          logger.info("Invite code: " ++ inviteCode);
          ServerStore.dispatch(AttachPrivatePlayer(sock_id, username, inviteCode, ack));

        | _ => 
          // todo: merge on and onWith ack so I don't have to ignore other messages like this
          ()
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
      let {
        ServerState.db_game,
        db_public_games_created,
        db_public_games_started,
        db_private_games_created,
        db_private_games_started,
        db_server_started_at,
      } =
        ServerStore.getState();
      let gameStates = StringMap.valuesToArray(db_game);
      <div>
        <div>
          <span> {"Server started at: " |> ReasonReact.string} </span>
          <span>{db_server_started_at->Js.Date.toISOString |> ReasonReact.string}</span>
        </div>
        <div>
          <span> {"Public games created: " |> ReasonReact.string} </span>
          <span> {db_public_games_created |> string_of_int |> ReasonReact.string} </span>
        </div>
        <div>
          <span> {"Public games started:" |> ReasonReact.string} </span>
          <span> {db_public_games_started |> string_of_int |> ReasonReact.string} </span>
        </div>
        <div>
          <span> {"Private games created: " |> ReasonReact.string} </span>
          <span> {db_private_games_created |> string_of_int |> ReasonReact.string} </span>
        </div>
        <div>
          <span> {"Private games started:" |> ReasonReact.string} </span>
          <span> {db_private_games_started |> string_of_int |> ReasonReact.string} </span>
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
