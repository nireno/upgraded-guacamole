open Belt
open AppPrelude

module StringMap = Map.String

let logger = appLogger.makeChild({"_context": "ServerState"})

// type user = {
//   usr_sock_id: string,
//   usr_name: string,
//   usr_maybe_game_id: option(Game.game_id)
// };

type playerGame = {
  sock_id: string,
  player_id: Player.id,
  game_key: game_key,
}

type db = {
  db_game: Map.String.t<Game.state> /* game_id -> Game.state */,
  db_game_timer: Map.String.t<Timer.timeout> /* game_key -> Timer.timeout */,
  db_server_started_at: Js.Date.t,
  db_public_games_created: int,
  db_private_games_created: int,
  db_public_games_started: int,
  db_private_games_started: int,
}

let getGameBySocket: (sock_id, db) => option<Game.state> = (sock_id, {db_game}) =>
  db_game
  ->StringMap.valuesToArray
  ->Belt.Array.getBy(game =>
    game.Game.clients->(
      Quad.exists(
        clientState =>
          switch clientState {
          | Game.Attached({client_socket_id}) if client_socket_id == sock_id => true

          | _ => false
          },
        /* Currently we're using getGameBySocket to map client actions to a game. But if
         * getGameBySocket is used to determine the mapping and we include games where the client
         * was once Attached but is now Detached, then you occasionally get a bug where a client action
         * is routed to the wrong game so that it is ignored; client game gets no update/feedback and
         * appears to be frozen/stuck/unresponsive.
         * To reproduce:
         * 1. Add the case commented out below back into the switch statement above.
         * 1. Have Client-A create a new public game (Game-A) and have 3 other players join, then deal the first card.
         * 1. Refresh Client-A's page so that the client leaves Game-A in FindingSubs phase.
         * 1. Having refreshed, let Client-A create another new public game Game-B, add 2 more players to Game-B
         *    but then have Client-A leave Game-B before adding the last player. This will leave Game-B in the FindingPlayers phase.
         * 1. Have Client-A join a public game. Client-A will initially join Game-B but click "Join as Substitute" button to join Game-A.
         * 1. Leave Game-A before the game starts.
         * 1. Now "Join Public Game" again - This will join you to Game-B again.
         * 1. Let the game start. Client-A should be the dealer. Attempt to deal cards.
         * 1. You should find that the cards are not dealt so the game appears to be frozen. */
        // | Game.Detached({client_socket_id}, _) if client_socket_id == sock_id => true
        _,
      )
    )
  )

let getGameClientSeat = (db_game, sock_id) => {
  let gameMaybe =
    db_game
    ->StringMap.valuesToArray
    ->Belt.Array.getBy(game => game.Game.clients->(Quad.exists(clientState =>
          switch clientState {
          | Game.Attached({client_socket_id}) if client_socket_id == sock_id => true
          | _ => false
          }
        , _)))
  switch gameMaybe {
  | None => None
  | Some(game) =>
    let clientSeatMaybe =
      game.clients
      ->Quad.withId
      ->Quad.find(((_sid, clientState)) =>
        switch clientState {
        | Attached({client_socket_id})
        | Detached({client_socket_id}, _) if client_socket_id == sock_id => true
        | _ => false
        }
      )

    switch clientSeatMaybe {
    | None => None
    | Some((seat_id, _clientState)) => Some((game, seat_id))
    }
  }
}

// let getGamesWhere: (~phase: Game.Filter.simplePhase=?, ~privacy: Game.Filter.privacy=?, db) => list(Game.state) =
//   (~phase as maybePhase=? , ~privacy=PrivateOrPublic, {db_game}) => {
//     let matchesPrivacy = gameState => {
//       switch (gameState.Game.game_id) {
//       | Public(_) when privacy == Public => true
//       | Private(_) when privacy == Private => true
//       | _ when privacy == PrivateOrPublic => true
//       | _ => false
//       };
//     };
//     let matchesPhase = gameState => {
//       switch (maybePhase) {
//       | None => true
//       | Some(phase) => phase == Game.Filter.simplifyPhase(gameState.Game.phase)
//       };
//     };
//     db_game
//     ->StringMap.valuesToArray
//     ->Array.keep(gameState => gameState->matchesPrivacy && gameState->matchesPhase)
//     ->List.fromArray;
//   };

type sock_id = string
type username = string
type inviteCode = string

let stringOfMsg = x =>
  switch x {
  | ServerEvent.AddGame(_game) => "AddGame"
  | RemoveGame(_game_id) => "RemoveGame"
  | ReplaceGame(_game_id, _state) => "ReplaceGame"
  | AttachPlayer(_data) => "AttachPlayer"
  | AttachPublicPlayer(_data) => "AttachPublicPlayer"
  | CreatePrivateGame({sock_id, client_username}) =>
    "CreatePrivateGame(" ++ (sock_id ++ ("," ++ (client_username ++ ")")))
  | AttachPrivatePlayer(_data) => "AttachPrivatePlayer"
  | RemovePlayerBySocket(_sock_id) => "RemovePlayerBySocket"
  | AttachSubstitute(_data) => "AttachSubstitute"
  | KickActivePlayer(_game_id) => "KickActivePlayer"
  | UpdateGame(_game_id, _action) => "UpdateGame"
  | UpdateGameBySocket(_sock_id, _action) => "UpdateGameBySocket"
  | TriggerEffects(_effects) => "TriggerEffects"
  | ReconcileSubstitution => "ReconcileSubstitution"
  | Rematch(_sock_id) => "Rematch"
  | RotateGuests(_sock_id) => "RotateGuests"
  | PrivateToPublic(_sock_id) => "PrivateToPublic"
  | FireGameTimer(_) => "FireGameTimer"
  | AddGameTimeout(_) => "AddGameTimeout"
  | RemoveGameTimeout(_) => "RemoveGameTimeout"
  | PublicGameStarted => "PublicGameStarted"
  | PrivateGameStarted => "PrivateGameStarted"
  | RelaySignal(_) => "RelaySignal"
  }

let empty = () => {
  db_game: Map.String.empty,
  db_game_timer: Map.String.empty,
  db_server_started_at: Js.Date.make(),
  db_public_games_created: 0,
  db_private_games_created: 0,
  db_public_games_started: 0,
  db_private_games_started: 0,
}

let buildClientState = (gameState, player, playerPhase) => {
  let getPartnerInfo = (gameState, playerId) => {
    let partnerId = Player.getPartner(playerId)
    let partner = Quad.get(partnerId, gameState.Game.players)
    {
      ClientGame.cardsToDisplay: partner.pla_hand->List.keep(card =>
        card.Card.rank == Card.Rank.Ace ||
          (card.rank == Card.Rank.King ||
          (card.rank == Card.Rank.Queen ||
            (card.rank == Card.Rank.Jack ||
            card.rank == Card.Rank.Ten)))
      ),
      trumpCount: switch gameState.maybeTrumpCard {
      | None => 0
      | Some(trumpCard) =>
        partner.pla_hand->List.keep(card => card.Card.suit == trumpCard.suit)->List.length
      },
    }
  }
  let playerHand = Quad.get(player, gameState.Game.players).pla_hand
  let handFacing = if Game.isFaceDownPhase(gameState.phase) {
    player == gameState.dealer || player == gameState.leader
      ? Hand.FaceUpHand(playerHand)
      : Hand.FaceDownHand(List.length(playerHand))
  } else {
    Hand.FaceUpHand(playerHand)
  }

  let getPlayerPublicState = (player: Game.playerData, client: Game.clientState) => {
    let getUserProfileMaybe = x =>
      switch x {
      | Game.Detached(_, _)
      | Vacant =>
        None
      | Attached(client) =>
        Some({
          ClientGame.client_username: client.client_username,
          client_identicon: client.client_id,
          client_initials: client.client_initials,
          client_profile_type: client.client_profile_type,
        })
      }
    open ClientGame
    {pla_card: player.pla_card, pla_profile_maybe: getUserProfileMaybe(client)}
  }

  open ClientGame
  {
    gameId: gameState.game_id,
    phase: playerPhase,
    gamePhase: gameState.phase->Join.clientGamePhaseOfGamePhase,
    players: gameState.clients
    ->Quad.zip(gameState.players)
    ->(Quad.map(((client, player)) => getPlayerPublicState(player, client), _)),
    teams: gameState.teams,
    me: player,
    maybePartnerInfo: switch gameState.phase {
    | PlayerTurnPhase(_n) => Some(getPartnerInfo(gameState, player))
    | _ => None
    },
    myTricks: Quad.get(player, gameState.players).pla_tricks,
    dealer: gameState.dealer,
    leader: gameState.leader,
    handFacing,
    maybeTrumpCard: gameState.maybeTrumpCard,
    maybeLeadCard: gameState.maybeLeadCard,
  }
}

let buildSocketStatePairs: Game.state => list<(option<sock_id>, ClientGame.state)> = gameState => {
  let decidePlayerPhase = Game.decidePlayerPhase(gameState.phase, gameState.dealer)

  let playerPhasePairs: list<(Player.id, Player.phase)> =
    list{Quad.N1, N2, N3, N4}->Belt.List.map(p => p->decidePlayerPhase)

  let buildClientState = buildClientState(gameState)
  playerPhasePairs->Belt.List.map(((playerId, playerPhase)) => (
    switch gameState.clients->(Quad.get(playerId, _)) {
    | Vacant
    | Detached(_, _) =>
      None
    | Attached(client) => Some(client.client_socket_id)
    },
    buildClientState(playerId, playerPhase),
  ))
}

let rec initPrivateGame = db_game => {
  let gameIdExists = (searchKey, gameState) =>
    switch gameState.Game.game_id {
    | Public(_) => false
    | Private({private_game_key: key}) => searchKey == key
    }
  let newGameState = Game.initPrivateGame()
  let stringOfGameId = newGameState.game_id->Game.stringOfGameId
  db_game->StringMap.valuesToArray->Array.some(gameIdExists(stringOfGameId))
    ? initPrivateGame(db_game)
    : newGameState
}

let rec update: (ServerEvent.event, db) => update<db, ServerEvent.effect> = (
  msg,
  {db_game, db_game_timer} as db,
) => {
  let logger = logger.makeChild({"_context": "ServerState.update", "update_msg": stringOfMsg(msg)})
  switch msg {
  | AddGame(game) =>
    // ensure game_id is not already in db_game
    let key = game.game_id->Game.stringOfGameId
    switch db_game->StringMap.get(key) {
    | None => Update({...db, db_game: db_game->StringMap.set(key, game)})
    | Some(_) => NoUpdate(db)
    }

  | RemoveGame(game_key) =>
    switch db_game->StringMap.get(game_key) {
    | None => NoUpdate(db)
    | Some(_) => Update({...db, db_game: db_game->StringMap.remove(game_key)})
    }

  | ReplaceGame(game_key, game) =>
    logger.debug2(Game.debugOfState(game), "Updating game")
    updateMany(
      list{
        ServerEvent.RemoveGame(game_key),
        AddGame(game),
        TriggerEffects(list{EmitStateByGame(game_key)}),
      },
      NoUpdate(db),
    )

  | AttachPlayer({
      game_key,
      sock_id,
      client_username,
      client_id,
      client_initials,
      client_profile_type,
    }) =>
    // let logger = logger.makeChild({"_context": "AttachPlayer", "sock_id"});
    switch db_game->StringMap.get(game_key) {
    | None => NoUpdate(db)
    | Some({game_id} as gameStatePrev) =>
      let findSeatByPrecedence = (clients, client_id) => {
        let wasClientAttached = (incoming_client_id, client) =>
          switch client {
          | (_id, Game.Attached({client_id}))
          | (_id, Detached({client_id}, _)) if client_id == incoming_client_id => true
          | _ => false
          }

        let isClientVacant = x =>
          switch x {
          | (_id, Game.Vacant) => true
          | _ => false
          }

        let isClientDetached = x =>
          switch x {
          | (_id, Game.Detached(_)) => true
          | _ => false
          }

        clients
        ->Quad.withId
        ->Quad.findByList(list{wasClientAttached(client_id), isClientVacant, isClientDetached})
      }

      switch findSeatByPrecedence(gameStatePrev.clients, client_id) {
      | None => NoUpdate(db)
      | Some((player_id, _)) =>
        let client_username = client_username == "" ? Player.stringOfId(player_id) : client_username
        let clientState = Game.Attached({
          Game.client_username,
          client_socket_id: sock_id,
          client_id,
          client_initials,
          client_profile_type,
          client_connected_at: Js.Date.now(),
        })

        let (gameStateNext, effects) = GameReducer.reduce(
          AttachClient(player_id, clientState),
          gameStatePrev,
        )

        let (db_public_games_started, db_private_games_started) = switch gameStatePrev.phase {
        | FindPlayersPhase({emptySeatCount: 0}) =>
          switch game_id {
          | Public(_) => (db.db_public_games_started + 1, db.db_private_games_started)
          | Private(_) => (db.db_public_games_started, db.db_private_games_started + 1)
          }
        | _ => (db.db_public_games_started, db.db_private_games_started)
        }

        updateMany(
          list{ReplaceGame(game_key, gameStateNext), TriggerEffects(effects)},
          Update({
            ...db,
            db_public_games_started,
            db_private_games_started,
          }),
        )
      }
    }
  | RemovePlayerBySocket(sock_id) =>
    // #LeaveGame
    let logger = logger.makeChild({"_context": "RemovePlayerBySocket", "sock_id": sock_id})
    switch db_game->getGameClientSeat(sock_id) {
    | None => NoUpdate(db)
    | Some((game, player_id)) =>
      let game_key = game.game_id->SharedGame.stringOfGameId
      switch StringMap.get(db_game, game_key) {
      | None => Update(db)
      | Some(gameForLeave) =>
        logger.debug2(
          {
            "player": {
              "seat_id": Player.stringOfId(player_id),
              "playerId": Player.stringOfId(player_id),
            },
            "game": Game.debugOfState(gameForLeave),
          },
          "Removing player from game.",
        )
        let (gameAftLeave, effects) = GameReducer.reduce(LeaveGame(player_id), gameForLeave)

        if (
          gameAftLeave.clients->Quad.countHaving(x =>
            switch x {
            | Attached(_) => true
            | _ => false
            }
          ) == 0
        ) {
          // Remove the game if there are no more connected clients
          updateMany(
            list{
              RemoveGame(game_key),
              TriggerEffects(list{ServerEvent.ResetClient(sock_id)}),
              ReconcileSubstitution,
            },
            Update(db),
          )
        } else {
          updateMany(
            list{
              ReplaceGame(game_key, gameAftLeave),
              TriggerEffects(
                Belt.List.concat(
                  list{ServerEvent.ResetClient(sock_id), ServerEvent.EmitStateByGame(game_key)},
                  effects,
                ),
              ),
              ReconcileSubstitution,
            },
            Update(db),
          )
        }
      }
    }
  | AttachPublicPlayer({
      sock_id,
      client_username,
      ack,
      client_id,
      client_initials,
      client_profile_type,
    }) =>
    let logger = logger.makeChild({
      "_context": "AttachPublicPlayer",
      "sock_id": sock_id,
      "client_username": client_username,
    })

    let findUnfilledPublicGame = db_game =>
      db_game
      ->StringMap.valuesToArray
      ->Belt.Array.getBy(game =>
        switch game.Game.game_id {
        | Private(_) => false
        | Public(_) =>
          switch game.phase {
          | FindPlayersPhase({emptySeatCount}) if emptySeatCount > 0 => true
          | _ => false
          }
        }
      )

    switch findUnfilledPublicGame(db_game) {
    | None =>
      logger.info("Attaching player to a new public game.")
      let nextGameId = db.db_public_games_created + 1
      let gameState = {...Game.initialState(), game_id: Public(nextGameId->string_of_int)}
      // let gameState = Game.TestState.initHangJackGame();
      let db' = {...db, db_public_games_created: db.db_public_games_created + 1}
      updateMany(
        list{
          AddGame(gameState),
          RemovePlayerBySocket(sock_id),
          AttachPlayer({
            game_key: gameState.game_id->SharedGame.stringOfGameId,
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
          ReconcileSubstitution,
          TriggerEffects(list{ServerEvent.EmitAck(ack, SocketMessages.AckOk)}),
        },
        Update(db'),
      )
    | Some(gameState) =>
      logger.info("Attaching player to an existing public game.")
      updateMany(
        list{
          RemovePlayerBySocket(sock_id),
          AttachPlayer({
            game_key: gameState.game_id->SharedGame.stringOfGameId,
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
          TriggerEffects(list{ServerEvent.EmitAck(ack, SocketMessages.AckOk)}),
        },
        NoUpdate(db),
      )
    }
  | CreatePrivateGame({
      sock_id,
      client_username,
      client_id,
      client_initials,
      client_profile_type,
    }) =>
    let findClientExistingGame = (sock_id, db_game) =>
      db_game->StringMap.findFirstBy((_k, {Game.clients: clients}) =>
        switch clients->Quad.find(clientState =>
          switch clientState {
          | Attached({client_socket_id}) if client_socket_id == sock_id => true
          | _ => false
          }
        ) {
        | None => false
        | Some(_) => true
        }
      )

    switch findClientExistingGame(sock_id, db_game) {
    | None =>
      let db' = {...db, db_private_games_created: db.db_private_games_created + 1}
      let gameState = initPrivateGame(db.db_game)
      updateMany(
        list{
          AddGame(gameState),
          AttachPlayer({
            game_key: gameState.game_id->SharedGame.stringOfGameId,
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
        },
        Update(db'),
      )
    | Some((game_key, _game)) =>
      logger.warn("CreatePrivateGame event from client already attached to existing game.")
      SideEffects(db, list{EmitStateByGame(game_key)})
    }
  | AttachPrivatePlayer({
      sock_id,
      client_username,
      client_id,
      client_initials,
      invite_code,
      ack,
      client_profile_type,
    }) =>
    // let logger = logger.makeChild({"_context": "AttachPrivatePlayer", "sock_id"});

    @ocaml.doc(" Find game having the provided invite code ")
    let gameMatchesCode = (inviteCode, gameState) =>
      switch gameState.Game.game_id {
      | Public(_) => false
      | Private({private_game_key: key}) =>
        Shared.normalizeInviteCode(key) == Shared.normalizeInviteCode(inviteCode)
      }

    let matchingGames = Belt.List.fromArray(
      (Belt.Array.keep(_, gameMatchesCode(invite_code)))(StringMap.valuesToArray(db_game)),
    )

    switch matchingGames {
    | list{gameState, ..._rest} =>
      updateMany(
        list{
          RemovePlayerBySocket(sock_id),
          AttachPlayer({
            game_key: gameState.game_id->SharedGame.stringOfGameId,
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
          TriggerEffects(list{ServerEvent.EmitAck(ack, SocketMessages.AckOk)}),
        },
        NoUpdate(db),
      )
    | _ =>
      SideEffects(
        db,
        list{
          ServerEvent.EmitAck(
            ack,
            SocketMessages.AckError("No games found with the provided invite code."),
          ),
        },
      )
    }

  | AttachSubstitute({sock_id, client_username, client_id, client_initials, client_profile_type}) =>
    switch db_game
    ->StringMap.valuesToArray
    ->Array.keep(game => game->Game.isPublic && game.phase->Game.isFindSubsPhase)
    ->List.fromArray {
    | list{} =>
      logger.warn("No game found to attach a substitute player")
      NoUpdate(db)
    | list{gameState, ..._rest} =>
      updateMany(
        list{
          RemovePlayerBySocket(sock_id),
          AttachPlayer({
            game_key: gameState.game_id->SharedGame.stringOfGameId,
            sock_id,
            client_username,
            client_id,
            client_initials,
            client_profile_type,
          }),
          ReconcileSubstitution,
        },
        NoUpdate(db),
      )
    }

  | KickActivePlayer(game_key) =>
    switch db_game->StringMap.get(game_key) {
    | None => NoUpdate(db)
    | Some(gameState) =>
      let maybeActivePlayer = ActivePlayer.find(gameState.Game.phase, gameState.dealer)
      switch maybeActivePlayer {
      | None => NoUpdate(db)
      | Some(activePlayer) =>
        switch Quad.get(activePlayer.id, gameState.clients) {
        | Vacant => NoUpdate(db)
        | Attached(client)
        | Detached(client, _) =>
          update(ServerEvent.RemovePlayerBySocket(client.client_socket_id), db)
        }
      }
    }

  | UpdateGameBySocket(sock_id, action) =>
    switch db->(getGameBySocket(sock_id, _)) {
    | None => NoUpdate(db)
    | Some({game_id}) =>
      let game_key = game_id->SharedGame.stringOfGameId
      update(UpdateGame(game_key, action), db)
    }
  | UpdateGame(game_key, action) =>
    // let logger = logger.makeChild({"_context": {j|UpdateGame($game_key, ...)|j}});
    switch db_game->StringMap.get(game_key) {
    | None => NoUpdate(db)
    | Some(gameState) =>
      let (nextGameState, effects) = GameReducer.reduce(action, gameState)

      let maybeGameStartedEvent = Game.isNewGameCheck(nextGameState)
        ? switch nextGameState.game_id {
          | Public(_) => Some(ServerEvent.PublicGameStarted)
          | Private(_) => Some(ServerEvent.PrivateGameStarted)
          }
        : None

      updateMany(
        list{
          ServerEvent.ReplaceGame(game_key, nextGameState),
          TriggerEffects(list{ServerEvent.EmitStateByGame(game_key), ...effects}),
        }->My.List.addSome(maybeGameStartedEvent),
        NoUpdate(db),
      )
    }

  | TriggerEffects(effects) => SideEffects(db, effects)

  | ReconcileSubstitution =>
    let currCanSub =
      db_game
      ->StringMap.valuesToArray
      ->Array.some(game => game->Game.Filter.hasPrivacy(Public) && game->Game.needsSubstitutes)

    let updates =
      db_game
      ->StringMap.valuesToArray
      ->Array.keepMap(game =>
        game->Game.Filter.hasPrivacy(Public)
          ? switch game.phase {
            | FindPlayersPhase({emptySeatCount, canSub}) if canSub != currCanSub =>
              Some(
                ServerEvent.ReplaceGame(
                  game.game_id->SharedGame.stringOfGameId,
                  {
                    ...game,
                    phase: FindPlayersPhase({emptySeatCount, canSub: currCanSub}),
                  },
                ),
              )
            | _ => None
            }
          : None
      )
      ->List.fromArray

    updateMany(updates, NoUpdate(db))

  | Rematch(sock_id) =>
    let logger = logger.makeChild({"_context": "Rematch", "sock_id": sock_id})
    logger.info("Got rematch signal")
    switch db_game->getGameClientSeat(sock_id) {
    | None => NoUpdate(db)
    | Some(({game_id}, player_id)) =>
      let game_key = game_id->SharedGame.stringOfGameId
      update(UpdateGame(game_key, PlayerRematch(player_id)), db)
    }

  | RotateGuests(sock_id) =>
    /* The game "host" is not part of the rotation */
    switch db_game->getGameClientSeat(sock_id) {
    | None => NoUpdate(db)
    | Some(({game_id}, _)) =>
      let game_key = game_id->SharedGame.stringOfGameId
      let rotateGuests = ({Game.game_id: game_id, clients: (p1, p2, p3, p4)} as game) =>
        switch game_id {
        | Private({private_game_host}) =>
          logger.debug(private_game_host->Quad.stringifyId)
          let rotateByHost = x =>
            switch x {
            | Quad.N1 => (p1, p4, p2, p3)
            | Quad.N2 => (p4, p2, p1, p3)
            | Quad.N3 => (p4, p1, p3, p2)
            | Quad.N4 => (p3, p1, p2, p4)
            }
          {...game, clients: rotateByHost(private_game_host)}
        | Public(_) => game
        }

      /* db_player_game caches the seat_id/player_id of players attached to a game.
       When players are rotated, this cache needs to be updated as well. */
      // let updatePlayerSeating = (db_player_game, clients) =>
      //   clients
      //   ->Quad.withId
      //   ->Quad.reduce(
      //       ((quadId, clientState), db_player_game) =>
      //         switch (clientState) {
      //         | Game.Vacant => db_player_game
      //         | Connected({Game.client_socket_id})
      //         | Disconnected({Game.client_socket_id}, _) =>
      //           db_player_game->StringMap.set(
      //             client_socket_id,
      //             {sock_id: client_socket_id, player_id: quadId, game_key},
      //           )
      //         },
      //       db_player_game,
      //     );

      let (db_game, _gameMaybe) = db_game->My.StringMap.update(game_key, rotateGuests)

      updateMany(list{TriggerEffects(list{EmitStateByGame(game_key)})}, Update({...db, db_game}))
    }

  | PrivateToPublic(sock_id) =>
    switch db_game->getGameClientSeat(sock_id) {
    | None => NoUpdate(db)
    | Some(({game_id}, _)) =>
      let game_key = game_id->SharedGame.stringOfGameId
      switch db_game->StringMap.get(game_key) {
      | None => NoUpdate(db)
      | Some(game) =>
        let (game, _effects) = GameReducer.reduce(PrivateToPublic, game)
        let db_game = db_game->StringMap.set(game_key, game)
        UpdateWithSideEffects({...db, db_game}, list{EmitStateByGame(game_key)})
      }
    }
  | FireGameTimer(sock_id) =>
    switch db_game->getGameClientSeat(sock_id) {
    | None => NoUpdate(db)
    | Some(({game_id}, _)) =>
      let game_key = game_id->SharedGame.stringOfGameId
      switch db_game_timer->StringMap.get(game_key) {
      | None => NoUpdate(db)
      | Some(timeout) =>
        switch timeout {
        | RunningTimeout(_, task, _, _)
        | PausedTimeout(task, _, _) =>
          task()
          NoUpdate(db)
        }
      }
    }

  | AddGameTimeout({ServerEvent.game_key: game_key, timeout}) =>
    // let timeout = Timer.startTimeout(() => update(event), delay_milliseconds);
    let update = x =>
      switch x {
      | None => Some(timeout)
      | Some(prevTimeout) =>
        Timer.clearTimeout(prevTimeout)
        Some(timeout)
      }
    let db_game_timerNext = db_game_timer->StringMap.update(game_key, update)
    Update({...db, db_game_timer: db_game_timerNext})

  | RemoveGameTimeout(game_key) =>
    /* By returning None in this update function, StringMap.update will actually remove the kv pair.
     See https://bucklescript.github.io/bucklescript/api/Belt.Map.html#VALupdate */
    let clearAndRemoveTimer = x =>
      switch x {
      | None => None
      | Some(timeout) =>
        Timer.clearTimeout(timeout)
        None
      }

    let db_game_timerNext = db_game_timer->StringMap.update(game_key, clearAndRemoveTimer)

    Update({...db, db_game_timer: db_game_timerNext})

  | PublicGameStarted => Update({...db, db_public_games_started: db.db_public_games_started + 1})

  | PrivateGameStarted => Update({...db, db_private_games_started: db.db_private_games_started + 1})

  | RelaySignal(sock_id, signal) =>
    switch getGameBySocket(sock_id, db) {
    | None => NoUpdate(db)
    | Some(game) =>
      let getQidOfSender = (clients, socketId) =>
        clients->Quad.findId(x =>
          switch x {
          | Game.Attached({client_socket_id}) if client_socket_id == socketId => true
          | _ => false
          }
        )

      switch game.clients->getQidOfSender(sock_id) {
      | None => NoUpdate(db)
      | Some(qid) =>
        let effects =
          game.clients
          ->Quad.toList
          ->Belt.List.keepMap(client =>
            switch client {
            | Attached({client_socket_id}) =>
              Some(ServerEvent.EmitSignal(client_socket_id, qid, signal))
            | _ => None
            }
          )
        SideEffects(db, effects)
      }
    }
  }
}
and updateResult: (
  ServerEvent.event,
  update<db, ServerEvent.effect>,
) => update<db, ServerEvent.effect> = (msg, result) =>
  switch result {
  | NoUpdate(db) => update(msg, db)
  | SideEffects(db, effects) =>
    switch update(msg, db) {
    | NoUpdate(_) => result
    | SideEffects(_, effectsOfUpdate) => SideEffects(db, List.concat(effects, effectsOfUpdate))
    | Update(dbAftUpdate) => UpdateWithSideEffects(dbAftUpdate, effects)
    | UpdateWithSideEffects(dbAftUpdate, effectsOfUpdate) =>
      UpdateWithSideEffects(dbAftUpdate, List.concat(effects, effectsOfUpdate))
    }
  | Update(db) =>
    switch update(msg, db) {
    | NoUpdate(_) => result
    | SideEffects(_, effectsOfUpdate) => UpdateWithSideEffects(db, effectsOfUpdate)
    | updateish => updateish
    }
  | UpdateWithSideEffects(db, effects) =>
    switch update(msg, db) {
    | NoUpdate(_) => result
    | SideEffects(_, effectsOfUpdate) =>
      UpdateWithSideEffects(db, List.concat(effects, effectsOfUpdate))
    | Update(dbAftUpdate) => UpdateWithSideEffects(dbAftUpdate, effects)
    | UpdateWithSideEffects(dbAftUpdate, effectsOfUpdate) =>
      UpdateWithSideEffects(dbAftUpdate, List.concat(effects, effectsOfUpdate))
    }
  }
and updateMany: (
  list<ServerEvent.event>,
  update<db, ServerEvent.effect>,
) => update<db, ServerEvent.effect> = (msgs, initialResult) =>
  msgs->Belt.List.reduce(initialResult, (result, msg) => updateResult(msg, result))
