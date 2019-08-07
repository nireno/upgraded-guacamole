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

type msg = 
  | AddGame(Game.state)
  | AddPlayerGame(sock_id, Player.id, Game.game_id)
  | RemoveGame(Game.game_id)
  | ReplaceGame(Game.game_id, Game.state)
  | AttachPlayer(Game.game_id, sock_id, username )
  | AttachPublicPlayer(sock_id, username)
  | CreatePrivateGame(sock_id, username)
  | AttachPrivatePlayer(sock_id, username, string, SocketMessages.serverToClient => unit)
  | RemovePlayerBySocket(sock_id)
  | AttachSubstitute(sock_id, username)
  | KickActivePlayer(Game.game_id)
  | InsertKickTimeoutId(Game.game_id, Js.Global.timeoutId)
  | DeleteKickTimeoutId(Game.game_id)
  | UpdateGame(Game.game_id, GameReducer.action)
  | UpdateGameBySocket(sock_id, GameReducer.action)
  | TriggerEffects(list(ServerEffect.effect))
  | ReconcileSubstitution
  ;

let stringOfMsg = fun
  | AddGame(_game) => "AddGame"
  | AddPlayerGame(_sock_id, _player_id, _game_id) => "AddPlayerGame"
  | RemoveGame(_game_id) => "RemoveGame"
  | ReplaceGame(_game_id, _state) => "ReplaceGame"
  | AttachPlayer(_game_id, _sock_id, _username ) => "AttachPlayer"
  | AttachPublicPlayer(_sock_id, _username) => "AttachPublicPlayer"
  | CreatePrivateGame(sock_id, username) => "CreatePrivateGame(" ++ sock_id ++ "," ++ username ++ ")"
  | AttachPrivatePlayer(_sock_id, _username, _invite_code, _ack) => "AttachPrivatePlayer"
  | RemovePlayerBySocket(_sock_id) => "RemovePlayerBySocket"
  | AttachSubstitute(_sock_id, _username) => "AttachSubstitute"
  | KickActivePlayer(_game_id) => "KickActivePlayer"
  | InsertKickTimeoutId(_game_id, _timeoutId) => "InsertKickTimeoutId"
  | DeleteKickTimeoutId(_game_id) => "DeleteKickTimeoutId"
  | UpdateGame(_game_id, _action) => "UpdateGame"
  | UpdateGameBySocket(_sock_id, _action) => "UpdateGameBySocket"
  | TriggerEffects(_effects) => "TriggerEffects"
  | ReconcileSubstitution => "ReconcileSubstitution"
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

  let p1State = Quad.get(N1, gameState.players);
  let p2State = Quad.get(N2, gameState.players);
  let p3State = Quad.get(N3, gameState.players);
  let p4State = Quad.get(N4, gameState.players);
  ClientGame.{
    gameId: gameState.game_id,
    phase: playerPhase,
    gamePhase: gameState.phase,
    players: (
      {pla_name: p1State.pla_name, pla_card: p1State.pla_card},
      {pla_name: p2State.pla_name, pla_card: p2State.pla_card},
      {pla_name: p3State.pla_name, pla_card: p3State.pla_card},
      {pla_name: p4State.pla_name, pla_card: p4State.pla_card},
    ),
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
    playerPhasePairs->Belt.List.map(((player, playerPhase)) =>
      (
        Quad.select(player, x => Game.(x.sock_id_maybe), gameState.players),
        buildClientState(player, playerPhase),
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

    | AttachPlayer(game_id, sock_id, username) =>
      let str_game_id = Game.(stringOfGameId(game_id));
      switch (db_game->StringMap.get(str_game_id)) {
      | None => NoUpdate(db)
      | Some({game_id} as gameState) =>
        switch (Game.findEmptySeat(gameState)) {
        | None => NoUpdate(db)
        | Some(player_id) =>
          let pla_name = username == "" ? Player.stringOfId(player_id) : username;
          let db_player_game' =
            StringMap.set(db_player_game, sock_id, {sock_id, player_id, game_id});
          let db' = {...db, db_player_game: db_player_game'};

          let players =
            Quad.update(
              player_id,
              player => Game.{...player, pla_name, sock_id_maybe: Some(sock_id)},
              gameState.players,
            );

          let playersNeeded = 4 - Game.countPlayers(players);
          let phase =
            switch (gameState.phase) {
            | FindSubsPhase(_n, subPhase) =>
              playersNeeded == 0 ? subPhase : FindSubsPhase(playersNeeded, subPhase)
            | FindPlayersPhase(_n, canSub) =>
              playersNeeded == 0 ? DealPhase : FindPlayersPhase(playersNeeded, canSub)
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

          let gameAftAttach = {...gameState, players, phase};
          let playerJoinedNotis =
            Noti.playerBroadcast(
              ~from=player_id,
              ~msg=Noti.Text(pla_name ++ " joined the game."),
              ~level=Noti.Success,
              (),
            );

          let clientToasts = 
            playerJoinedNotis->List.keepMap(noti =>
              Quad.select(
                noti.noti_recipient,
                player =>
                  switch (player.Game.sock_id_maybe) {
                  | None => None
                  | Some(sock_id) => Some({sock_id, ServerEffect.toast: noti})
                  },
                gameAftAttach.players,
              )
            );

          updateMany(
            [
              ReplaceGame(gameState.game_id, gameAftAttach),
              TriggerEffects([EmitClientToasts(clientToasts)]),
              TriggerEffects(
                gameAftAttach.phase->SharedGame.isPlayerActivePhase
                  ? [ServerEffect.ResetKickTimeout(game_id)] : [],
              ),
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
                "username":
                  Quad.select(player_id, player => player.Game.pla_name, gameForLeave.players),
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

          if (Game.playerCount(gameAftLeave) == 0) {
            // Remove the game if it becomes empty
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
    | AttachPublicPlayer(sock_id, username) =>
      let logger =
        logger.makeChild({"_context": "AttachPublicPlayer", "sock_id": sock_id, "username": username});

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
            AttachPlayer(gameState.game_id, sock_id, username),
            ReconcileSubstitution,
          ],
          Update(db'),
        );
      | [gameState, ..._rest] => 
        logger.info("Attaching player to an existing public game.");
        updateMany(
          [
            RemovePlayerBySocket(sock_id), 
            AttachPlayer(gameState.game_id, sock_id, username)
          ],
          NoUpdate(db),
        );
      };
    | CreatePrivateGame(sock_id, username) =>
      let db' = {...db, db_private_games_created: db.db_private_games_created + 1};
      let gameState = initPrivateGame(db.db_game);
      updateMany(
        [
          AddGame(gameState), 
          AttachPlayer(gameState.game_id, sock_id, username)
        ],
        Update(db'),
      );
    | AttachPrivatePlayer(sock_id, username, inviteCode, ack) =>
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
        |> Belt.Array.keep(_, gameMatchesCode(inviteCode))
        |> Belt.List.fromArray;

      switch (matchingGames) {
      | [gameState, ..._rest] =>
        updateMany(
          [
            RemovePlayerBySocket(sock_id),
            AttachPlayer(gameState.game_id, sock_id, username),
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

    | AttachSubstitute(sock_id, username) =>
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
              AttachPlayer(gameState.game_id, sock_id, username),
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
          switch (Quad.get(activePlayer.id, gameState.players).Game.sock_id_maybe) {
          | None => NoUpdate(db)
          | Some(sock_id) => update(RemovePlayerBySocket(sock_id), db)
          }
        };
      }

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
