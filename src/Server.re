[%%debugger.chrome];
open AppPrelude;

[%%raw "require('dotenv').config()"]
[@bs.val] external nodeEnv: string = "process.env.NODE_ENV";
[@bs.val] external httpPortEnv: Js.Nullable.t(string) = "process.env.allfours_port";
[@bs.val] external adminPasswordEnv: Js.Nullable.t(string) = "process.env.allfours_admin_password";

[@bs.module] external nanoid: unit => string = "nanoid";

let logger = appLogger.makeChild({"_module": "Server"});

let app = Express.express();

Express.App.useOnPath(
  app,
  ~path="/",
  Express.Static.(make("./build", defaultOptions()) |> asMiddleware),
);

let expressStaticOptions = Express.Static.defaultOptions();

// Clients should only make a request to check for changes to /static files 
// after the maxAge has elapsed.
let cacheMaxAgeMillis = daysToMillis(1);
expressStaticOptions->Express.Static.immutable(true);  
expressStaticOptions->Express.Static.maxAge(cacheMaxAgeMillis); 

Express.App.useOnPath(
  app,
  ~path="/static",
  Express.Static.(make("./static", expressStaticOptions) |> asMiddleware),
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

let actionOfIO_Action: SocketMessages.clientToServer => Game.event =
  fun
  | IO_JoinGame(_)
  | IO_StartPrivateGame(_)
  | IO_JoinPrivateGame(_)
  | IO_LeaveGame
  | IO_PlayAgain(_)
  | IO_Rematch
  | IO_PrivateToPublic
  | IO_RotateGuests
  | IO_Substitute(_) => Noop
  | IO_TransitionGameNow => Noop
  
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

    let isHandshakeOk =
      SocketServer.Socket.getHandshake(socket)->SocketServer.Handshake.isHandshakeOk(~clientVersion="0.2.1");
    if(isHandshakeOk){
      SocketServer.Socket.on(
        socket,
        io => {
          let stringOfEvent = SocketMessages.stringOfClientToServer(io);
          let logger = logger.makeChild({"_context": "socket-onevent", "event": stringOfEvent});
          logger.debug2("Handling `%s` event. ", stringOfEvent);
          switch (io) {
          | IO_StartPrivateGame(client_username, ioClientSettingsJson) =>
            let {ClientSettings.client_id, client_initials, client_profile_type} =
              decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
            ServerStore.dispatch(CreatePrivateGame({sock_id, client_username, client_id, client_initials, client_profile_type}));
            logger.info2(getGameStats(), "Game stats:");

          | IO_JoinPrivateGame(_inviteCode, _client_username, _ioClientSettings) => () // handled later by Socket.onWithAck

          | IO_JoinGame(_client_username, _ioClientSettingsJson) => () // handled later by Socket.onWithAck

          | IO_LeaveGame =>
            ServerStore.dispatch(RemovePlayerBySocket(sock_id));
            logger.info2(getGameStats(), "Game stats:");

          | IO_PlayAgain(client_username, ioClientSettingsJson) =>
            let {ClientSettings.client_id, client_initials, client_profile_type} =
              decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettingsJson);
            switch (ServerStore.getGameBySocket(sock_id)) {
            | None => ServerStore.dispatch(TriggerEffects([ServerEvent.ResetClient(sock_id)]))
            | Some(gameState) =>
              switch (gameState.game_id) {
              | Public(_) =>
                let noAck = _ => ()
                ServerStore.dispatchMany([
                  RemovePlayerBySocket(sock_id),
                  AttachPublicPlayer({sock_id, client_username, client_id, client_initials, ack: noAck, client_profile_type}),
                ])
              | Private(_) =>
                ServerStore.dispatchMany([
                  RemovePlayerBySocket(sock_id),
                  CreatePrivateGame({sock_id, client_username, client_id, client_initials, client_profile_type})]);
              }
            };
            logger.info2(getGameStats(), "Game stats:");

          | IO_Substitute(client_username, ioClientSettings) => 
            let {ClientSettings.client_id, client_initials, client_profile_type} =
              decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings);
            ServerStore.dispatch(AttachSubstitute({sock_id, client_username, client_id, client_initials, client_profile_type}));

          | IO_Rematch => ServerStore.dispatch(Rematch(sock_id))
          | IO_RotateGuests => ServerStore.dispatch(RotateGuests(sock_id))
          | IO_TransitionGameNow => ServerStore.dispatch(FireGameTimer(sock_id))
          | IO_PrivateToPublic => ServerStore.dispatch(PrivateToPublic(sock_id))
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
          | IO_JoinPrivateGame(invite_code, client_username, ioClientSettings) =>
            let {ClientSettings.client_id, client_initials, client_profile_type} =
              decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings);
            logger.info("Invite code: " ++ invite_code);

            ServerStore.dispatch(
              AttachPrivatePlayer({sock_id, client_username, client_id, client_initials, invite_code, ack, client_profile_type}),
            );
          
          | IO_JoinGame(client_username, ioClientSettings) =>
            let {ClientSettings.client_id, client_initials, client_profile_type} =
              decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings);
            ServerStore.dispatch(AttachPublicPlayer({sock_id, client_username, client_id, client_initials, ack, client_profile_type}));
            logger.info2(getGameStats(), "Game stats:");
          | _ => 
            // todo: merge on and onWith ack so I don't have to ignore other messages like this
            ()
          };
        },
      );
      SocketServer.Store.dispatch(AddSocket(socket));
      logger.info("Socket connected");
      onSocketDisconnect(socket);
      SocketServer.Socket.emit(socket, SocketMessages.Reset);
    } else {
      logger.warn2("Handshake failed. Disconnecting socket.", socket->SocketServer.Socket.getId);
      SocketServer.Socket.emit(socket, SocketMessages.HandshakeFailed);
      socket->SocketServer.Socket.disconnect(true) |> ignore;
    }


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

Http.listen(http, httpPort, () => logger.info("Listening on *:" ++ string_of_int(httpPort)));
