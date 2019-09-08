open Belt;
open AppPrelude;

module StringMap = Map.String;

let logger = appLogger.makeChild({"_context": "ServerState"})

  // type user = {
  //   usr_sock_id: string,
  //   usr_name: string,
  //   usr_maybe_game_id: option(Game.game_id)
  // };

type playerGame = {
  sock_id: string,
  player_id: Player.id,
  game_id: Game.game_id,
}

type db = {
  db_game: Map.String.t(Game.state), /* game_id -> Game.state */ 
  db_player_game: Map.String.t(playerGame), /* sock_id -> playerGame */
  db_server_started_at: Js.Date.t,
  db_public_games_created: int,
  db_private_games_created: int,
  db_public_games_started: int,
  db_private_games_started: int,
};

let getGameBySocket: (sock_id, db) => option(Game.state) =
  (sock_id, {db_game, db_player_game}) => {
    switch (db_player_game->StringMap.get(sock_id)) {
    | None => None
    | Some({game_id}) => db_game->StringMap.get(game_id->Game.stringOfGameId)
    };
  };




let getGamesWhere: (~phase: Game.Filter.phase=?, ~privacy: Game.Filter.privacy=?, db) => list(Game.state) =
  (~phase as maybePhase=? , ~privacy=PrivateOrPublic, {db_game}) => {
    let matchesPrivacy = gameState => {
      switch (gameState.Game.game_id) {
      | Public(_) when privacy == Public => true
      | Private(_) when privacy == Private => true
      | _ when privacy == PrivateOrPublic => true
      | _ => false
      };
    };
    let matchesPhase = gameState => {
      switch (maybePhase) {
      | None => true
      | Some(phase) => phase == Game.Filter.simplifyPhase(gameState.Game.phase)
      };
    };
    db_game
    ->StringMap.valuesToArray
    ->Array.keep(gameState => gameState->matchesPrivacy && gameState->matchesPhase)
    ->List.fromArray;
  };

type sock_id = string;
type username = string;
type inviteCode = string

type attachPlayerData = {
  game_id: Game.game_id,
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
};

type attachPublicPlayerData = {
  sock_id: string,
  client_username: string,
  ack: SocketMessages.ack,
  client_id: string,
  client_initials: string,
};

type attachPrivatePlayerData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
  invite_code: string,
  ack: SocketMessages.ack,
};

type createPrivateGameData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
};

type attachSubstituteData = {
  sock_id: string,
  client_username: string,
  client_id: string,
  client_initials: string,
};

type msg = 
  | AddGame(Game.state)
  | AddPlayerGame(sock_id, Player.id, Game.game_id)
  | RemoveGame(Game.game_id)
  | ReplaceGame(Game.game_id, Game.state)
  | AttachPlayer(attachPlayerData)
  | AttachPublicPlayer(attachPublicPlayerData)
  | CreatePrivateGame(createPrivateGameData)
  | AttachPrivatePlayer(attachPrivatePlayerData)
  | RemovePlayerBySocket(sock_id)
  | AttachSubstitute(attachSubstituteData)
  | KickActivePlayer(Game.game_id)
  | InsertKickTimeoutId(Game.game_id, Js.Global.timeoutId)
  | DeleteKickTimeoutId(Game.game_id)
  | UpdateGame(Game.game_id, GameReducer.action)
  | UpdateGameBySocket(sock_id, GameReducer.action)
  | TriggerEffects(list(ServerEffect.effect))
  | ReconcileSubstitution
  | IdleWithTimeout(Game.game_id, Timer.timeout)
  | Rematch(sock_id)
  | SelectPartner(sock_id)
  | StartGameNow(sock_id) //short circuits the usual delay before game starts
  ;

let stringOfMsg = fun
  | AddGame(_game) => "AddGame"
  | AddPlayerGame(_sock_id, _player_id, _game_id) => "AddPlayerGame"
  | RemoveGame(_game_id) => "RemoveGame"
  | ReplaceGame(_game_id, _state) => "ReplaceGame"
  | AttachPlayer(_data) => "AttachPlayer"
  | AttachPublicPlayer(_data) => "AttachPublicPlayer"
  | CreatePrivateGame({sock_id, client_username}) => "CreatePrivateGame(" ++ sock_id ++ "," ++ client_username ++ ")"
  | AttachPrivatePlayer(_data) => "AttachPrivatePlayer"
  | RemovePlayerBySocket(_sock_id) => "RemovePlayerBySocket"
  | AttachSubstitute(_data) => "AttachSubstitute"
  | KickActivePlayer(_game_id) => "KickActivePlayer"
  | InsertKickTimeoutId(_game_id, _timeoutId) => "InsertKickTimeoutId"
  | DeleteKickTimeoutId(_game_id) => "DeleteKickTimeoutId"
  | UpdateGame(_game_id, _action) => "UpdateGame"
  | UpdateGameBySocket(_sock_id, _action) => "UpdateGameBySocket"
  | TriggerEffects(_effects) => "TriggerEffects"
  | ReconcileSubstitution => "ReconcileSubstitution"
  | IdleWithTimeout(_, _) => "IdleWithTimeout"
  | Rematch(_sock_id) => "Rematch"
  | SelectPartner(_sock_id) => "RotatePartner"
  | StartGameNow(_sock_id) => "StartGameNow"
  ;


let empty = () => {
  db_game: Map.String.empty,
  db_player_game: Map.String.empty,
  db_server_started_at: Js.Date.make(),
  db_public_games_created: 0,
  db_private_games_created: 0,
  db_public_games_started: 0,
  db_private_games_started: 0,
};


let buildClientState = (gameState, player, playerPhase) => {
  let getPartnerInfo = (gameState, playerId) => {
    let partnerId = Player.getPartner(playerId);
    let partner = Quad.get(partnerId, gameState.Game.players);
    {
      ClientGame.cardsToDisplay:
        partner.pla_hand
        ->List.keep(card =>
            card.Card.rank == Card.Rank.Ace
            || card.rank == Card.Rank.King
            || card.rank == Card.Rank.Queen
            || card.rank == Card.Rank.Jack
            || card.rank == Card.Rank.Ten
          ),
      trumpCount:
        switch (gameState.maybeTrumpCard) {
        | None => 0
        | Some(trumpCard) =>
          partner.pla_hand->List.keep(card => card.Card.suit == trumpCard.suit)->List.length
        },
    };
  };
  let playerHand = Quad.get(player, gameState.Game.players).pla_hand;
  let handFacing =
    if (SharedGame.isFaceDownPhase(gameState.phase)) {
      player == gameState.dealer || player == gameState.leader
        ? Hand.FaceUpHand(playerHand) : Hand.FaceDownHand(List.length(playerHand));
    } else {
      Hand.FaceUpHand(playerHand);
    };

  let getPlayerPublicState = (player: Game.playerData, client: Game.clientState) => {
    let getUserProfileMaybe =
      fun
      | Game.Vacant => None
      | Connected(client)
      | Disconnected(client, _) =>
        Some({
          ClientGame.client_username: client.client_username,
          client_identicon: client.client_id,
          client_initials: client.client_initials,
        });
    ClientGame.{pla_card: player.pla_card, pla_profile_maybe: getUserProfileMaybe(client)};
  };
  
  ClientGame.{
    gameId: gameState.game_id,
    phase: playerPhase,
    gamePhase: gameState.phase,
    players:
      gameState.clients
      ->Quad.zip(gameState.players)
      ->Quad.map(((client, player)) => getPlayerPublicState(player, client), _),
    teams: gameState.teams,
    me: player,
    maybePartnerInfo:
      switch (gameState.phase) {
      | PlayerTurnPhase(_n) => Some(getPartnerInfo(gameState, player))
      | _ => None
      },
    myTricks: Quad.get(player, gameState.players).pla_tricks,
    dealer: gameState.dealer,
    leader: gameState.leader,
    handFacing,
    maybeTrumpCard: gameState.maybeTrumpCard,
    maybeLeadCard: gameState.maybeLeadCard,
  };
};

let buildSocketStatePairs: Game.state => list((option(sock_id), ClientGame.state)) =
  gameState => {
    let decidePlayerPhase = Game.decidePlayerPhase(gameState.phase, gameState.dealer);

    let playerPhasePairs: list((Player.id, Player.phase)) =
      [Quad.N1, N2, N3, N4]->Belt.List.map(p => p->decidePlayerPhase);

    let buildClientState = buildClientState(gameState);
    playerPhasePairs->Belt.List.map(((playerId, playerPhase)) =>
      (
        switch (gameState.clients->Quad.get(playerId, _)) {
        | Vacant
        | Disconnected(_, _) => None
        | Connected(client) => Some(client.client_socket_id)
        },
        buildClientState(playerId, playerPhase),
      )
    );
  };

type reconcileKickTimeout =
  | Keep // Keep the current timer
  | Clear // Clear the current timer
  | Reset // Clear the current timer and start a new one
  ;

let reconcileKickTimeout = (prevGameState, nextGameState) => {
  let prevMaybeActivePlayer = ActivePlayer.find(prevGameState.Game.phase, prevGameState.dealer);
  let nextMaybeActivePlayer = ActivePlayer.find(nextGameState.Game.phase, nextGameState.dealer);
  if (prevMaybeActivePlayer != nextMaybeActivePlayer) {
    switch (nextMaybeActivePlayer) {
    | None => Clear
    | Some(_nextActivePlayer) =>
      Reset
    };
  } else {
    Keep;
  };
}

let rec initPrivateGame = (db_game) => {
  let gameIdExists = (searchId, gameState) =>
    switch (gameState.Game.game_id) {
    | Public(_) => false
    | Private(id) => searchId == id
    };
  let newGameState = Game.initPrivateGame();
  let stringOfGameId = newGameState.game_id->Game.stringOfGameId;
  db_game->StringMap.valuesToArray->Array.some(gameIdExists(stringOfGameId))
    ? initPrivateGame(db_game) : newGameState;
};

let rec update: (msg, db) => update(db, ServerEffect.effect) =
  (msg, {db_game, db_player_game} as db) => {
    let logger = logger.makeChild({"_context": "ServerState.update", "update_msg": stringOfMsg(msg)});
    switch (msg) {
    | AddGame(game) =>
      // ensure game_id is not already in db_game
      let key = game.game_id->Game.stringOfGameId;
      switch (db_game->StringMap.get(key)) {
      | None => Update({...db, db_game: db_game->StringMap.set(key, game)})
      | Some(_) => NoUpdate(db)
      };

    | AddPlayerGame(sock_id, player_id, game_id) =>
      let db = {
        ...db,
        db_player_game: db_player_game->StringMap.set(sock_id, {sock_id, player_id, game_id}),
      };
      Update(db);

    | RemoveGame(game_id) =>
      let key = game_id->Game.stringOfGameId;
      switch (db_game->StringMap.get(key)) {
      | None => NoUpdate(db)
      | Some(_) => Update({...db, db_game: db_game->StringMap.remove(key)})
      };

    | ReplaceGame(game_id, game) =>
      logger.debug2(Game.debugOfState(game), "Updating game");
      updateMany(
        [RemoveGame(game_id), AddGame(game), TriggerEffects([EmitStateByGame(game_id)])],
        NoUpdate(db),
      )

    | AttachPlayer({game_id, sock_id, client_username, client_id, client_initials}) =>
      let str_game_id = Game.(stringOfGameId(game_id));
      switch (db_game->StringMap.get(str_game_id)) {
      | None => NoUpdate(db)
      | Some({game_id} as gameState) =>
        switch (Game.findEmptySeat(gameState)) {
        | None => NoUpdate(db)
        | Some(player_id) =>
          let client_username = client_username == "" ? Player.stringOfId(player_id) : client_username;
          let db_player_game' =
            StringMap.set(db_player_game, sock_id, {sock_id, player_id, game_id});
          let db' = {...db, db_player_game: db_player_game'};

          let clients =
            Quad.update(
              player_id,
              _ =>
                Game.Connected({
                  Game.client_username: client_username,
                  client_socket_id: sock_id,
                  client_id,
                  client_initials,
                  client_connected_at: Js.Date.now(),
                }),
              gameState.clients,
            );

          let playersNeeded = clients->Quad.countHaving(clientState => clientState == Vacant); 
          let phase =
            switch (gameState.phase) {
            | FindSubsPhase(_n, IdlePhase(Some(timeout))) when playersNeeded == 0 =>
                SharedGame.IdlePhase(Some(Timer.restartTimeout(timeout)))
            | FindSubsPhase(_n, subPhase) =>
              playersNeeded == 0 ? subPhase : FindSubsPhase(playersNeeded, subPhase)
            | FindPlayersPhase(_n, canSub) => FindPlayersPhase(playersNeeded, canSub)
            | otherPhase => otherPhase
            };

          // Track how many games were actually started by looking at those that went 
          // specifically from the FindPlayersPhase to the DealPhase
          let db' =
            switch (gameState.phase) {
            | FindPlayersPhase(_, _) when phase == DealPhase =>
              switch (gameState.game_id) {
              | Public(_) => {...db', db_public_games_started: db'.db_public_games_started + 1}
              | Private(_) => {...db', db_private_games_started: db'.db_private_games_started + 1}
              }
            | _ => db'
            };

          let gameAftAttach = {...gameState, clients, phase};
          let playerJoinedNotis =
            Noti.playerBroadcast(
              ~from=player_id,
              ~msg=Noti.Text(client_username ++ " joined the game."),
              ~level=Noti.Success,
              (),
            );

          let clientToasts =
            playerJoinedNotis->List.keepMap(noti =>
              gameAftAttach.clients
              ->Quad.select(
                  noti.noti_recipient,
                  fun
                  | Game.Connected(client) =>
                    Some({sock_id: client.client_socket_id, ServerEffect.toast: noti})
                  | _ => None,
                  _,
                )
            );
          
          let effects =
            [
              Some(ServerEffect.EmitClientToasts(clientToasts))
            , gameAftAttach.phase->SharedGame.isPlayerActivePhase
                ? Some(ServerEffect.ResetKickTimeout(game_id)) : None
            , playersNeeded == 0
                ? Some(
                    ServerEffect.IdleThenUpdateGame({
                      game_id,
                      game_reducer_action: StartGame,
                      idle_milliseconds: 12->secondsToMillis,
                    }),
                  )
                : None,
            ]
            ->Belt.List.keepMap(identity);

          updateMany(
            [
              ReplaceGame(gameState.game_id, gameAftAttach),
              TriggerEffects(effects),
            ],
            Update(db'),
          );
        }
      };
    | RemovePlayerBySocket(sock_id) =>
      let logger = logger.makeChild({"_context": "LeaveGame", "sock_id": sock_id});
      switch (StringMap.get(db_player_game, sock_id)) {
      | None => NoUpdate(db)
      | Some({player_id, game_id}) =>
        let db_player_game' = db_player_game->StringMap.remove(sock_id);
        let dbAftRemove_player_game = {...db, db_player_game: db_player_game'};
        switch (StringMap.get(db_game, game_id->SharedGame.stringOfGameId)) {
        | None => Update(dbAftRemove_player_game)
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
          );
          My.Global.clearMaybeTimeout(gameForLeave.maybeKickTimeoutId);
          let gameAftLeave = GameReducer.reduce(LeaveGame(player_id), gameForLeave);
          let playerLeftToasts =
            gameAftLeave.notis->List.keepMap(gameAftLeave->Join.mapNotiToSocketMaybe);

          let gameAftLeave = GameReducer.reduce(ClearNotis, gameAftLeave);

          if (gameAftLeave.clients
              ->Quad.countHaving(
                  fun
                  | Connected(_) => true
                  | _ => false,
                )
              == 0) {
            // Remove the game if there are no more connected clients
            updateMany(
              [
                RemoveGame(gameAftLeave.game_id),
                TriggerEffects([ServerEffect.ResetClient(sock_id)]),
                ReconcileSubstitution,
              ],
              Update(dbAftRemove_player_game),
            );
          } else {
            updateMany(
              [
                ReplaceGame(gameAftLeave.game_id, gameAftLeave),
                TriggerEffects([
                  ServerEffect.ResetClient(sock_id),
                  ServerEffect.EmitStateByGame(gameAftLeave.game_id),
                  ServerEffect.EmitClientToasts(playerLeftToasts),
                ]),
                ReconcileSubstitution,
              ],
              Update(dbAftRemove_player_game),
            );
          };
        };
      };
    | AttachPublicPlayer({sock_id, client_username, ack, client_id, client_initials}) =>
      let logger =
        logger.makeChild({"_context": "AttachPublicPlayer", "sock_id": sock_id, "client_username": client_username});

      switch (getGamesWhere(~privacy=Public, ~phase=FindPlayersPhase, db)) {
      | [] =>
        logger.info("Attaching player to a new public game.");
        let nextGameId = db.db_public_games_created + 1;
        let gameState = {...Game.initialState(), game_id: Public(nextGameId->string_of_int)};
        // let gameState = Game.TestState.initHangJackGame();
        let db' = {...db, db_public_games_created: db.db_public_games_created + 1};
        updateMany(
          [
            AddGame(gameState),
            RemovePlayerBySocket(sock_id),
            AttachPlayer({game_id: gameState.game_id, sock_id, client_username, client_id, client_initials}),
            ReconcileSubstitution,
            TriggerEffects([ServerEffect.EmitAck(ack, SocketMessages.AckOk)]),
          ],
          Update(db'),
        );
      | [gameState, ..._rest] => 
        logger.info("Attaching player to an existing public game.");
        updateMany(
          [
            RemovePlayerBySocket(sock_id), 
            AttachPlayer({game_id: gameState.game_id, sock_id, client_username, client_id, client_initials}),
            TriggerEffects([ServerEffect.EmitAck(ack, SocketMessages.AckOk)]),
          ],
          NoUpdate(db),
        );
      };
    | CreatePrivateGame({sock_id, client_username, client_id, client_initials}) =>
      let db' = {...db, db_private_games_created: db.db_private_games_created + 1};
      let gameState = initPrivateGame(db.db_game);
      updateMany(
        [
          AddGame(gameState), 
          AttachPlayer({game_id: gameState.game_id, sock_id, client_username, client_id, client_initials})
        ],
        Update(db'),
      );
    | AttachPrivatePlayer({sock_id, client_username, client_id, client_initials, invite_code, ack}) =>
      /** Find game having the provided invite code */
      let gameMatchesCode = (inviteCode, gameState) => {
        let normalizeCode = code =>
          // Lowercases all letters and removes spaces
          Js.String.toLowerCase(code) |> Js.String.replaceByRe([%re "/ /g"], "");
        switch (gameState.Game.game_id) {
        | Public(_) => false
        | Private(code) => normalizeCode(code) == normalizeCode(inviteCode)
        };
      };

      let matchingGames =
        StringMap.valuesToArray(db_game)
        |> Belt.Array.keep(_, gameMatchesCode(invite_code))
        |> Belt.List.fromArray;

      switch (matchingGames) {
      | [gameState, ..._rest] =>
        updateMany(
          [
            RemovePlayerBySocket(sock_id),
            AttachPlayer({game_id: gameState.game_id, sock_id, client_username, client_id, client_initials}),
            TriggerEffects([ServerEffect.EmitAck(ack, SocketMessages.AckOk)]),
          ],
          NoUpdate(db),
        )
      | _ =>
        SideEffects(
          db,
          [ServerEffect.EmitAck(ack, SocketMessages.AckError("No games found with the provided invite code."))],
        )
      };

    | AttachSubstitute({sock_id, client_username, client_id, client_initials}) =>
        switch (
          db_game
          ->StringMap.valuesToArray
          ->Array.keep(game => game->Game.isPublic && game.phase->Game.isFindSubsPhase)
          ->List.fromArray
        ) {
        | [] =>
          logger.warn("No game found to attach a substitute player");
          NoUpdate(db);
        | [gameState, ..._rest] =>
          updateMany(
            [
              RemovePlayerBySocket(sock_id),
              AttachPlayer({game_id: gameState.game_id, sock_id, client_username, client_id, client_initials}),
              ReconcileSubstitution,
            ],
            NoUpdate(db),
          )
        };

    | KickActivePlayer(game_id) =>
      switch (db_game->StringMap.get(game_id->Game.stringOfGameId)) {
      | None => NoUpdate(db)
      | Some(gameState) =>
        let maybeActivePlayer = ActivePlayer.find(gameState.Game.phase, gameState.dealer);
        switch (maybeActivePlayer) {
        | None => NoUpdate(db)
        | Some(activePlayer) =>
          switch (Quad.get(activePlayer.id, gameState.clients)) {
          | Vacant => NoUpdate(db)
          | Connected(client)
          | Disconnected(client, _) => update(RemovePlayerBySocket(client.client_socket_id), db)
          }
        };
      };

    | InsertKickTimeoutId(game_id, timeoutId) =>
      switch (db_game->StringMap.get(game_id->Game.stringOfGameId)) {
      | None => NoUpdate(db)
      | Some(gameState) =>
        let db_game =
          db_game->StringMap.set(
            game_id->Game.stringOfGameId,
            {...gameState, maybeKickTimeoutId: Some(timeoutId)},
          );
        Update({...db, db_game});
      }

    | DeleteKickTimeoutId(game_id) =>
      switch (db_game->StringMap.get(game_id->Game.stringOfGameId)) {
      | None => NoUpdate(db)
      | Some(gameState) =>
        let db_game =
          db_game->StringMap.set(
            game_id->Game.stringOfGameId,
            {...gameState, maybeKickTimeoutId: None},
          );
        Update({...db, db_game});
      }

    | UpdateGameBySocket(sock_id, action) =>
      switch (db_player_game->StringMap.get(sock_id)) {
      | None => NoUpdate(db)
      | Some({game_id}) => update(UpdateGame(game_id, action), db)
      }
    | UpdateGame(game_id, action) =>
      switch (db_game->StringMap.get(game_id->Game.stringOfGameId)) {
      | None => NoUpdate(db)
      | Some(gameState) =>
        let {Game.game_id} as gameAftAction = GameReducer.reduce(action, gameState);

        let maybeKickEffect =
          switch (reconcileKickTimeout(gameState, gameAftAction)) {
          | Keep => None
          | Clear => Some(ServerEffect.ClearKickTimeout(game_id))
          | Reset => Some(ResetKickTimeout(game_id))
          };


        let maybeAdvanceRound = {
          // Check if all players have a pla_card on the board to know if this is the
          // end of the trick.
          let isEndTrick =
            Quad.map(
              player => Js.Option.isSome(player.Game.pla_card) ? true : false,
              gameAftAction.players,
            )
            |> Quad.foldLeftUntil(
                 (iPlayed, wePlayed) => wePlayed && iPlayed,
                 wePlayed => !wePlayed,
                 true,
               );
          isEndTrick ? Some(ServerEffect.DelayThenAdvanceRound(game_id)) : None;
        };
        
        let toasts = gameAftAction.notis->List.keepMap(Join.mapNotiToSocketMaybe(gameAftAction));
        let gameAftAction'clearNotis = GameReducer.reduce(ClearNotis, gameAftAction);

        let maybeEmitToastsEffect = switch(toasts){
        | [] => None
        | toasts => Some(ServerEffect.EmitClientToasts(toasts))
        }

        let someEffects = [maybeAdvanceRound, maybeKickEffect, maybeEmitToastsEffect]->List.keepMap(identity);

        updateMany(
          [
            ReplaceGame(gameState.game_id, gameAftAction'clearNotis),
            TriggerEffects([EmitStateByGame(game_id), ...someEffects]),
          ],
          NoUpdate(db),
        );
      }

    | TriggerEffects(effects) =>
      SideEffects(db, effects);

    | ReconcileSubstitution =>
      let currCanSub =
        db_game
        ->StringMap.valuesToArray
        ->Array.some(game =>
            game->Game.Filter.hasPrivacy(Public) && game->Game.Filter.hasPhase(FindSubsPhase)
          );

      let updates =
        db_game
        ->StringMap.valuesToArray
        ->Array.keepMap(game =>
            game->Game.Filter.hasPrivacy(Public)
              ? switch (game.phase) {
                | FindPlayersPhase(n, canSub) when canSub != currCanSub =>
                  Some(
                    ReplaceGame(
                      game.game_id,
                      {...game, phase: FindPlayersPhase(n, currCanSub)},
                    ),
                  )
                | _ => None
                }
              : None
          )
        ->List.fromArray;

      updateMany(updates, NoUpdate(db));
    | IdleWithTimeout(game_id, timeout) => 
      let db_game =
        db_game->StringMap.update(
          game_id->SharedGame.stringOfGameId, 
          Belt.Option.map(_, game => {...game, phase: IdlePhase(Some(timeout))})
        );
      Update({...db, db_game});

    | Rematch(sock_id) =>
      let logger = logger.makeChild({"_context": "Rematch", "sock_id": sock_id});
      logger.info("Got rematch signal");
      switch (db_player_game->StringMap.get(sock_id)) {
      | None => NoUpdate(db)
      | Some({game_id, player_id}) => 
        let updateGame = game => {
          switch (game.Game.phase) {
          | GameOverPhase(rematchDecisions) =>
            let rematchDecisions = rematchDecisions->Quad.put(player_id, SharedGame.RematchAccepted, _);
            // When all players have chosen to rematch or leave, reinit the game with the rematching players.
            // This may mean that the game goes into the FindPlayersPhase if some players left the game instead of
            // rematching. Or it may go into the deal phase if all players chose to rematch.

            let numRematchingPlayers =
              rematchDecisions->Quad.countHaving(decision => decision == RematchAccepted);

            let phase =
              if (rematchDecisions->Quad.every(decision => decision != SharedGame.RematchUnknown, _)) {
                numRematchingPlayers == 4
                  ? SharedGame.DealPhase : FindPlayersPhase(4 - numRematchingPlayers, false);
              } else {
                GameOverPhase(rematchDecisions);
              };

            switch (phase) {
            | GameOverPhase(_) => {...game, phase}
            | phase => {
                ...Game.initialState(),
                game_id: game.game_id,
                players: Quad.make(_ => Game.initPlayerData()),
                clients: game.clients,
                phase,
              }
            };
            | _ => game
          };
        };

        let db_game =
          db_game->StringMap.update(game_id->SharedGame.stringOfGameId, Belt.Option.map(_, updateGame));
          
        updateMany([TriggerEffects([EmitStateByGame(game_id)])], Update({...db, db_game}));
      }
    | SelectPartner(sock_id) =>
      switch (db_player_game->StringMap.get(sock_id)) {
      | None => NoUpdate(db)
      | Some({game_id, player_id}) =>
        if (player_id == N1) {
          let selectPartner = ({Game.clients: (p1, p2, p3, p4)} as game) => {
            ...game,
            clients: (p1, p4, p2, p3),
          };
          let db_game =
            db_game->StringMap.update(game_id->SharedGame.stringOfGameId, __x =>
              Belt.Option.map(__x, selectPartner)
            );

          updateMany([TriggerEffects([EmitStateByGame(game_id)])], Update({...db, db_game}));
        } else {
          NoUpdate(db);
        }
      };
    | StartGameNow(sock_id) =>
      // Start game now preflight
      // Check that player is in a game
      //   and that game is in the Findplayers(0, _) state
      // Clear any pending timers meant to fire StartGame
      logger.info("Recieved StartGameNow");
      switch(db_player_game->StringMap.get(sock_id)){
      | None => NoUpdate(db)
      | Some({game_id}) =>
        switch(db_game->StringMap.get(game_id->Game.stringOfGameId)){
        | None => NoUpdate(db)
        | Some(game) =>
          switch(game.phase){
          | IdlePhase(Some(timeout)) => 
            switch(timeout){
            | PausedTimeout(task, _, _)
            | RunningTimeout(_, task, _, _) => 
              task()
              NoUpdate(db)
            }
          | _ => NoUpdate(db)
          }
        }
      }
    };
  }
and updateResult: (msg, update(db, ServerEffect.effect)) => update(db, ServerEffect.effect) =
  (msg, result) => {
    switch (result) {
    | NoUpdate(db) => update(msg, db)
    | SideEffects(db, effects) =>
      switch (update(msg, db)) {
      | NoUpdate(_) => result
      | SideEffects(_, effectsOfUpdate) =>
        SideEffects(db, List.concat(effects, effectsOfUpdate))
      | Update(dbAftUpdate) => UpdateWithSideEffects(dbAftUpdate, effects)
      | UpdateWithSideEffects(dbAftUpdate, effectsOfUpdate) =>
        UpdateWithSideEffects(dbAftUpdate, List.concat(effects, effectsOfUpdate))
      }
    | Update(db) =>
      switch (update(msg, db)) {
      | NoUpdate(_) => result
      | SideEffects(_, effectsOfUpdate) => UpdateWithSideEffects(db, effectsOfUpdate)
      | updateish => updateish
      }
    | UpdateWithSideEffects(db, effects) =>
      switch (update(msg, db)) {
      | NoUpdate(_) => result
      | SideEffects(_, effectsOfUpdate) =>
        UpdateWithSideEffects(db, List.concat(effects, effectsOfUpdate))
      | Update(dbAftUpdate) => UpdateWithSideEffects(dbAftUpdate, effects)
      | UpdateWithSideEffects(dbAftUpdate, effectsOfUpdate) =>
        UpdateWithSideEffects(dbAftUpdate, List.concat(effects, effectsOfUpdate))
      }
    };
  }
and updateMany: (list(msg), update(db, ServerEffect.effect)) => update(db, ServerEffect.effect) =
  (msgs, initialResult) =>
    msgs->Belt.List.reduce(initialResult, (result, msg) => updateResult(msg, result));
