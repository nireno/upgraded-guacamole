open AppPrelude
open Belt

@module external nanoid: unit => string = "nanoid"

let logger = appLogger.makeChild({"_module": "Server"})

let app = Express.expressCjs()

Express.useWithPath(
  app,
  "/",
  {
    Express.staticMiddleware("./build")
  },
)

// Clients should only make a request to check for changes to /static files
// after the maxAge has elapsed.
let cacheMaxAgeMillis = daysToMillis(1)
let expressStaticOptions = {
  "immutable": true,
  "maxAge": cacheMaxAgeMillis,
}

Express.useWithPath(
  app,
  "/static",
  {
    Express.staticMiddlewareWithOptions("./build", expressStaticOptions)
  },
)

Express.useWithPath(
  app,
  "/static",
  {
    Express.staticMiddlewareWithOptions(
      "./static",
      {
        "immutable": true,
        "maxAge": "30d",
      },
    )
  },
)

let http = Http.create(app)

let io = SocketServer.createWithHttpAndOption(
  http,
  SocketServer.makeOptions(~pingInterval=55000, ()),
)
let ns = SocketServer.Ns.of_(io, "/")

// let getKeysToRooms = () =>
//   SocketServer.Ns.getAdapter(ns) |> Obj.magic |> BsSocketExtra.Adapter.rooms;

module StringMap = Belt.Map.String

let getGameStats = () => {
  let {ServerState.db_game: db_game, db_public_games_created} = ServerStore.getState()
  {
    "games-active": StringMap.size(db_game),
    "public-games-since-epoch": db_public_games_created,
  }
}

let actionOfIO_Action: SocketMessages.clientToServer => Game.Action.t = x =>
  switch x {
  | IO_JoinGame(_)
  | IO_StartPrivateGame(_)
  | IO_JoinPrivateGame(_)
  | IO_LeaveGame
  | IO_PlayAgain(_)
  | IO_Rematch
  | IO_PrivateToPublic
  | IO_RotateGuests
  | IO_Signal(_)
  | IO_Substitute(_) =>
    Noop
  | IO_TransitionGameNow => Noop

  | IO_PlayCard(ioPlayerId, ioCard) =>
    switch Player.id_decode(ioPlayerId |> Js.Json.parseExn) {
    | Belt.Result.Error(_) => Noop
    | Belt.Result.Ok(playerId) =>
      switch Card.t_decode(ioCard |> Js.Json.parseExn) {
      | Belt.Result.Error(_) => Noop
      | Belt.Result.Ok(card) => PlayCard(playerId, card)
      }
    }
  | IO_Beg => Beg
  | IO_Stand => Stand
  | IO_GiveOne => GiveOne
  | IO_Deal => Deal
  | IO_RunPack => RunPack
  | IO_FlipFinalTrump => FlipAgain
  | IO_DealAgain => DealAgain
  }

let onSocketDisconnect = socket =>
  SocketServer.Socket.onDisconnect(socket, () => {
    @ocaml.doc(" Note: by this time socketio has already removed the socket from the room.
        Also, during testing I noticed that refreshing the app in chrome rapidly multiple times
        sometimes results in socketio not immediately detecting a disconnect. However it 
        seems to triage after about a minute(?) so there is a delay between when you expect to 
        see a disconnect and when socketio finally fires this disconnect handler.
        
        The result is that there may be open game rooms holding sockets that are effectively dead.")
    let socketId = SocketServer.Socket.getId(socket)
    let logger = logger.makeChild({"_context": "socket-onDisconnect", "socketId": socketId})

    logger.info("Socket disconnected.")
    ServerStore.dispatch(RemovePlayerBySocket(socketId))
    SocketServer.Store.dispatch(RemoveSocket(socket))
    logger.info2(getGameStats(), "Game stats:")
  })

SocketServer.onConnect(io, socket => {
  let sock_id = SocketServer.Socket.getId(socket)
  let logger = logger.makeChild({"_context": "socket-onconnect", "sock_id": sock_id})

  let isHandshakeOk =
    SocketServer.Socket.getHandshake(socket)->SocketServer.Handshake.isHandshakeOk(
      ~clientVersion=Shared.packageJson["version"],
    )
  if isHandshakeOk {
    SocketServer.Socket.onWithAck(socket, (io, ack) => {
      let stringOfEvent = SocketMessages.stringOfClientToServer(io)
      let logger = logger.makeChild({"_context": "socket-onevent-with-ack", "event": stringOfEvent})
      logger.debug2("Handling `%s` event. ", stringOfEvent)

      switch io {
      | IO_StartPrivateGame(client_username, ioClientSettingsJson) =>
        let {
          ClientSettings.client_id: client_id,
          client_initials,
          client_profile_type,
        } = decodeWithDefault(
          ClientSettings.t_decode,
          ClientSettings.defaults,
          ioClientSettingsJson,
        )
        ServerStore.dispatch(
          CreatePrivateGame({
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
        )
        logger.info2(getGameStats(), "Game stats:")

      | IO_JoinPrivateGame(invite_code, client_username, ioClientSettings) =>
        let {
          ClientSettings.client_id: client_id,
          client_initials,
          client_profile_type,
        } = decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings)
        logger.info("Invite code: " ++ invite_code)

        ServerStore.dispatch(
          AttachPrivatePlayer({
            sock_id,
            client_username,
            client_id,
            client_initials,
            invite_code,
            ack,
            client_profile_type,
          }),
        )

      | IO_JoinGame(client_username, ioClientSettings) =>
        let {
          ClientSettings.client_id: client_id,
          client_initials,
          client_profile_type,
        } = decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings)
        ServerStore.dispatch(
          AttachPublicPlayer({
            sock_id,
            client_username,
            client_id,
            client_initials,
            ack,
            client_profile_type,
          }),
        )
        logger.info2(getGameStats(), "Game stats:")

      | IO_LeaveGame =>
        ServerStore.dispatch(RemovePlayerBySocket(sock_id))
        logger.info2(getGameStats(), "Game stats:")

      | IO_PlayAgain(client_username, ioClientSettingsJson) =>
        let {
          ClientSettings.client_id: client_id,
          client_initials,
          client_profile_type,
        } = decodeWithDefault(
          ClientSettings.t_decode,
          ClientSettings.defaults,
          ioClientSettingsJson,
        )
        switch ServerStore.getGameBySocket(sock_id) {
        | None => ServerStore.dispatch(TriggerEffects(list{ServerEvent.ResetClient(sock_id)}))
        | Some(gameState) =>
          switch gameState.game_id {
          | Public(_) =>
            let noAck = _ => ()
            ServerStore.dispatchMany(list{
              RemovePlayerBySocket(sock_id),
              AttachPublicPlayer({
                sock_id,
                client_username,
                client_id,
                client_initials,
                ack: noAck,
                client_profile_type,
              }),
            })
          | Private(_) =>
            ServerStore.dispatchMany(list{
              RemovePlayerBySocket(sock_id),
              CreatePrivateGame({
                sock_id,
                client_username,
                client_id,
                client_initials,
                client_profile_type,
              }),
            })
          }
        }
        logger.info2(getGameStats(), "Game stats:")

      | IO_Substitute(client_username, ioClientSettings) =>
        let {
          ClientSettings.client_id: client_id,
          client_initials,
          client_profile_type,
        } = decodeWithDefault(ClientSettings.t_decode, ClientSettings.defaults, ioClientSettings)
        ServerStore.dispatch(
          AttachSubstitute({
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
        )

      | IO_Rematch => ServerStore.dispatch(Rematch(sock_id))
      | IO_RotateGuests => ServerStore.dispatch(RotateGuests(sock_id))
      | IO_TransitionGameNow => ServerStore.dispatch(FireGameTimer(sock_id))
      | IO_PrivateToPublic => ServerStore.dispatch(PrivateToPublic(sock_id))
      | IO_Signal(signal) => ServerStore.dispatch(RelaySignal(sock_id, signal))
      | ioAction =>
        let action = ioAction |> actionOfIO_Action
        ServerStore.dispatch(UpdateGameBySocket(sock_id, action))
      }
    })
    SocketServer.Store.dispatch(AddSocket(socket))
    logger.info("Socket connected")
    onSocketDisconnect(socket)
    SocketServer.Socket.emit(socket, SocketMessages.Reset)
  } else {
    logger.warn2("Handshake failed. Disconnecting socket.", socket->SocketServer.Socket.getId)
    SocketServer.Socket.emit(socket, SocketMessages.HandshakeFailed)
    socket->SocketServer.Socket.disconnect(true) |> ignore
  }
})

switch Js.Nullable.toOption(ServerEnv.adminPasswordEnv) {
| None =>
  failwith(
    "An environment variable 'allfours_admin_password' must be provided.\nThis defines the password for the admin dashboard.",
  )
| Some(adminPassword) =>
  Express.get(app, "/dashboard", Raw.authMiddleware("admin", adminPassword)->Obj.magic)
  Express.get(app, "/dashboard", (_req, res) => {
    let {
      ServerState.db_game: db_game,
      db_public_games_created,
      db_public_games_started,
      db_private_games_created,
      db_private_games_started,
      db_server_started_at,
    } = ServerStore.getState()
    let gameStates = StringMap.valuesToArray(db_game)
    let activeGames =
      gameStates->Array.keep(x => {x.clients |> Quad.every(y => y->Game.isClientAttached)})

    /*
     * Filters out the stalled games from the gameStates array.
     * A game is considered stalled if all clients seats were taken but at least one client is detached.
     */
    let stalledGames = gameStates->Array.keep(x => {
      x.clients |> Quad.every(y => !(y->Game.isClientVacant)) &&
        x.clients |> Quad.exists(y => y->Game.isClientDetached)
    })

    let bootingGames = gameStates->Array.keep(x => {
      x.clients |> Quad.exists(y => y->Game.isClientVacant)
    })

    let html = <div>
      <div>
        <span> {"Server started at: " |> React.string} </span>
        <span> {db_server_started_at->Js.Date.toISOString |> React.string} </span>
      </div>
      <div>
        <span> {"Public games created: " |> React.string} </span>
        <span> {db_public_games_created |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Public games started:" |> React.string} </span>
        <span> {db_public_games_started |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Private games created: " |> React.string} </span>
        <span> {db_private_games_created |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Private games started:" |> React.string} </span>
        <span> {db_private_games_started |> string_of_int |> React.string} </span>
      </div>
      <hr />
      <div>
        <span> {"Total Active/Booting/Stalled Games: " |> React.string} </span>
        <span> {Array.length(gameStates) |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Currently Stalled games: " |> React.string} </span>
        <span> {Array.length(stalledGames) |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Currently Booting games: " |> React.string} </span>
        <span> {Array.length(bootingGames) |> string_of_int |> React.string} </span>
      </div>
      <div>
        <span> {"Currently Active games: " |> React.string} </span>
        <span> {Array.length(activeGames) |> string_of_int |> React.string} </span>
      </div>
      <h1> {"Active Games"->React.string} </h1>
      <DashView gameStates=activeGames />
      <h1> {"Booting Games"->React.string} </h1>
      <DashView gameStates=bootingGames />
      <h1> {"Stalled Games"->React.string} </h1>
      <DashView gameStates=stalledGames />
    </div> |> ReactDOMServer.renderToStaticMarkup
    Express.send(res, html)->ignore
  })
// })
}

@ocaml.doc(" 
 Add catchall for requests that should be handled by react's router
 (ReasonReactRouter) on the client side
 ")
Express.get(app, "/*", (_req, res) => Express.redirect(res, "/")->ignore)

let httpPort =
  Js.Nullable.toOption(ServerEnv.httpPortEnv) |> Js.Option.getWithDefault("3000") |> int_of_string

Http.listen(http, httpPort, () => logger.info("Listening on *:" ++ string_of_int(httpPort)))
