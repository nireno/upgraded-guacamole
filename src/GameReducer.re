open AppPrelude;
open Game;

let logger = appLogger.makeChild({"_context": "GameReducer"});


let getTeamHighAndLowMaybes:
  ((Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t), option(Card.t)) =>
  (option(AllFours.GameAward.luckyAwardData), option(AllFours.GameAward.luckyAwardData)) =
  ((player1Hand, player2Hand, player3Hand, player4Hand), maybeTrumpCard) => {
    switch (maybeTrumpCard) {
    | None => (None, None)
    | Some({Card.suit: trumpSuit}) =>
      let playerCards =
        List.flatten([
          player1Hand->Belt.List.map(card => (Quad.N1, card)),
          player2Hand->Belt.List.map(card => (Quad.N2, card)),
          player3Hand->Belt.List.map(card => (Quad.N3, card)),
          player4Hand->Belt.List.map(card => (Quad.N4, card))]
        );

      let playersTrumpAsc: list((Player.id, Card.t)) =
        playerCards
        |> List.filter(((_, {Card.suit})) => suit == trumpSuit)
        |> List.sort(((_, {Card.rank: rank1}), (_, {rank: rank2})) => compare(rank1, rank2));

      let playersTrumpDesc = List.rev(playersTrumpAsc);
      
      let getTeamHeadCardMaybe = (team_id, playerCards) =>
        switch (
          playerCards
          ->Belt.List.keep(((player_id, _card)) => player_id->teamOfPlayer == team_id)
          ->Belt.List.head
        ) {
        | None => None
        | Some((_, card)) => Some(card)
        };

      let maybeTeam1High = getTeamHeadCardMaybe(T1, playersTrumpDesc);
      let maybeTeam2High = getTeamHeadCardMaybe(T2, playersTrumpDesc);
      let maybeTeam1Low = getTeamHeadCardMaybe(T1, playersTrumpAsc);
      let maybeTeam2Low = getTeamHeadCardMaybe(T2, playersTrumpAsc);

      let maybeTeamLuckyAward = (maybeTeam1Card, maybeTeam2Card, cmp) =>
        switch (maybeTeam1Card, maybeTeam2Card) {
        | (None, None) => None
        | (None, Some(card)) => Some(GameAward.{team_id: T2, winning_card: card, losing_card_maybe: None})
        | (Some(card), None) => Some({team_id: T1, winning_card: card, losing_card_maybe: None})
        | (Some(card1), Some(card2)) =>
          cmp(card1.rank->Card.Rank.intOfRank, card2.rank->Card.Rank.intOfRank)
            ? Some({team_id: T1, winning_card: card1, losing_card_maybe: Some(card2)})
            : Some({team_id: T2, winning_card: card2, losing_card_maybe: Some(card1)})
        };

      let maybeTeamHighAward = maybeTeamLuckyAward(maybeTeam1High, maybeTeam2High, (>));
      let maybeTeamLowAward = maybeTeamLuckyAward(maybeTeam1Low, maybeTeam2Low, (<));

      (maybeTeamHighAward, maybeTeamLowAward);
    };
  };


let maybeAddHighPoint = ((state, awards)) =>
  switch (state.maybeTeamHigh) {
  | None => (state, awards)
  | Some({team_id} as highAwardData) => 
    let state = {
      ...state,
      teams:
        GameTeams.update(
          team_id,
          x => {...x, team_score: x.team_score + Ruleset.default.highAwardValue},
          state.teams,
        ),
    };
    let (_, l, j, g) = awards;
    (state, (Some(highAwardData), l, j, g))
  };

let maybeAddLowPoint = (( state, awards) ) =>
  switch (state.maybeTeamLow) {
  | None => (state, awards)
  | Some({team_id} as lowAwardData) => 
    let state = {
      ...state,
      teams:
        GameTeams.update(
          team_id,
          x => {...x, team_score: x.team_score + Ruleset.default.lowAwardValue},
          state.teams,
        ),
    };
    let (h, _, j, g) = awards;
    (state, (h, Some(lowAwardData), j, g))
  };

let maybeAddJackPoints = (( state, awards )) =>
  switch (state.maybeTeamJack) {
  | None => (state, awards)
  | Some({team_id, jack_award_type} as jackAwardData) => 
    let state = {
      ...state,
      teams:
        GameTeams.update(
          team_id,
          x => {...x, team_score: x.team_score + jack_award_type->GameAward.jackAwardValue},
          state.teams,
        ),
    };
    let (h, l, _, g) = awards;
    (state, (h, l, Some(jackAwardData), g))
  };

let getKickTrumpNotis = (maybeTrumpCard) =>
  switch (maybeTrumpCard) {
  | Some(trumpCard) when kickPoints(trumpCard.Card.rank) > 0 =>
    let trumpCardText = trumpCard->Card.stringOfCard;
    let kickPointsText = kickPoints(trumpCard.rank)->string_of_int;
    let msg = {j|Dealer kicked $trumpCardText:  +$kickPointsText|j};
    Noti.broadcast(~msg=Text(msg), ~kind=Confirm, ());
  | _ => []
  };


module ValidatePlay = {
  type playFailure =
    | WaitForTurn
    | AlreadyPlayed
    | CardNotInHand
    | CantUnderTrump
    | MustFollowSuit
    | MustFollowTrumpedSuit;

  // TODO - Still need to handle CantUnderTrump and MustFollowSuit
  let validate =
      (
        game_phase,
        game_leader_id,
        game_followsuit_maybe,
        pla_hand,
        playerId,
        cardPlayed,
        cardMaybesOnBoard,
        leadSuitMaybe,
        trumpSuit,
      ) => {
    let pla_card = cardMaybesOnBoard->Quad.get(playerId, _);
    let willUnderTrump = (cardPlayed: Card.t, pla_hand, cardMaybesOnBoard, leadSuitMaybe, trumpSuit) =>
      // the card-being-played *is* trump
      cardPlayed.suit == trumpSuit
      // the lead-suit is *not* trump
      && leadSuitMaybe->Belt.Option.mapWithDefault(false, suit => suit != trumpSuit)
      // There is a trump card on the board that ranks higher than the card being played
      && cardMaybesOnBoard->Quad.exists(
                              Belt.Option.mapWithDefault(_, false, ({Card.suit, rank}) =>
                                suit == trumpSuit
                                && rank->Card.Rank.intOfRank > cardPlayed.rank->Card.Rank.intOfRank
                              ),
                              _,
                            )
      // The player is still holding a non-trump card
      && pla_hand->List.exists(({Card.suit}) => suit != trumpSuit, _);

    game_phase != PlayerTurnPhase(playerId)
      ? Belt.Result.Error(WaitForTurn)
      : pla_card != None
          ? Belt.Result.Error(AlreadyPlayed)
          : !List.exists(card => card == cardPlayed, pla_hand)
              ? Belt.Result.Error(CardNotInHand)
              : willUnderTrump(cardPlayed, pla_hand, cardMaybesOnBoard, leadSuitMaybe, trumpSuit)
                  ? Belt.Result.Error(CantUnderTrump)
                  : (
                    switch (game_followsuit_maybe) {
                    | Some(suitToFollow)
                        when
                          game_leader_id == playerId
                          && trumpSuit != cardPlayed.Card.suit
                          && cardPlayed.suit != suitToFollow =>
                      Belt.Result.Error(MustFollowTrumpedSuit)
                    | _ => Ok()
                    }
                  );
  };
};

/* Handle things that take effect only when leaving a particular state */
let addLeaveStateEffects = (statePrev, (stateNext, effects)) => {
  statePrev.phase == stateNext.phase
    ? (stateNext, effects)
    : {
      let game_key = statePrev.game_id->SharedGame.stringOfGameId;
      switch (statePrev.phase) {
      | FindPlayersPhase({emptySeatCount: 4})
      | FindSubsPhase({emptySeatCount: 4}) =>
        // Discard the game start timer
        (stateNext, [ServerEvent.DiscardGameTimer(game_key), ...effects])

      | phase when phase->Game.isPlayerActivePhase =>
        /* At the moment I assume there is only 1 concurrent timer in effect for a game
           at any given moment. So If I'm leaving an active player phase I just clear whatever timeout
           is there and assume it was the kick timeout */
        (stateNext, effects @ [ServerEvent.DiscardGameTimer(game_key)])
      | _ => (stateNext, effects)
      };
    };
};

let addEnterStateEffects = (statePrev, (stateNext, effects)) => {
  statePrev.phase == stateNext.phase
    ? (stateNext, effects)
    : {
      let game_key = statePrev.game_id->SharedGame.stringOfGameId;
      switch (stateNext.phase) {
      | phase when phase->Game.isPlayerActivePhase => 
      
        (
          stateNext,
          effects @ [ServerEvent.CreateGameTimer(game_key, KickInactiveClientCountdown)],
        )
      | _ => (stateNext, effects)
      };
    };
};

/* 
  This module should probably be called GameMachine and this function should
  be the gameState machine. It takes some (before) state and an action and
  produces some (after) state state.
*/
let reduce = (action, state) => {
  let game_key = state.game_id->SharedGame.stringOfGameId;

  let rec reduceRec: 
    (Game.event, Game.state, list(ServerEvent.effect)) => (Game.state, list(ServerEvent.effect)) = 
    (action, state, effects) => {
    switch (action) {
    | Noop => (state, effects)
    | NewRound =>
      /* what used to be EndRound start */
      let team1Tricks =
        Quad.get(N1, state.players).pla_tricks 
        @ Quad.get(N3, state.players).pla_tricks;
      let team2Tricks =
        Quad.get(N2, state.players).pla_tricks 
        @ Quad.get(N4, state.players).pla_tricks;

      let calcPoints = tricks =>
        tricks
        |> List.map(Quad.toList)
        |> List.concat
        |> List.fold_left(
              (acc, {Card.rank}) => acc + Card.Rank.pointsOfRank(rank),
              0,
            );
      
      let team1Points = calcPoints(team1Tricks);
      let team2Points = calcPoints(team2Tricks);

      let maybeTeamGame =
        team1Points == team2Points
          ? Some({GameAward.team_id_maybe: None, winning_count: team1Points, losing_count: team2Points})
          : team1Points > team2Points
              ? Some({
                  GameAward.team_id_maybe: Some(Team.T1),
                  winning_count: team1Points,
                  losing_count: team2Points,
                })
              : Some({team_id_maybe: Some(Team.T2), winning_count: team2Points, losing_count: team1Points});

      let maybeAddGamePoint = (maybeTeamGame: option(GameAward.gameAwardData), (state, awards)) =>
        switch (maybeTeamGame) {
        | Some({GameAward.team_id_maybe: Some(T1 as team_id)} as gameAwardData)
        | Some({GameAward.team_id_maybe: Some(T2 as team_id)} as gameAwardData) => 
          let state = {
            ...state,
            teams:
              GameTeams.update(
                team_id,
                x => {...x, team_score: x.team_score + Ruleset.default.gameAwardValue},
                state.teams,
              ),
          };
          let (h, l, j, _) = awards;
          (state, (h, l, j, Some(gameAwardData)))
        | Some(gameAwardData) =>
          let (h, l, j, _) = awards;
          (state, (h, l, j, Some(gameAwardData)))
        | _ => ( state, awards )
        };

      let (state, (h, l, j, g)) = 
        Util.updateUntil(
          [
            maybeAddHighPoint,
            maybeAddLowPoint,
            maybeAddJackPoints,
            maybeAddGamePoint(maybeTeamGame),
          ],
          ((state, _)) => isGameOverTest(state),
          (state, (None, None, None, None)),
        );

      let nextDealer = Quad.nextId(state.dealer);

      let notiEffects =
        Noti.broadcast(
          ~msg=RoundSummary(
            { noti_maybeTeamHigh: h
            , noti_maybeTeamLow: l
            , noti_maybeTeamJack: j
            , noti_maybeTeamGame: g
            }
          ),
          ~kind=Confirm,
          (),
        )->Belt.List.map(noti =>
             ServerEvent.NotifyPlayer(state.game_id->SharedGame.stringOfGameId, noti)
           );

      let effects =
        effects->Belt.List.concat(notiEffects);

      ({...state, 
        phase: isGameOverTest(state) ? GameOverPhase(Quad.make(_ => RematchUnknown)) : DealPhase,
        deck: Deck.make() |> Deck.shuffle,
        players: Quad.map(x => {...x, pla_tricks: []}, state.players),
        dealer: nextDealer,
        maybeTrumpCard: None,
        maybeLeadCard: None,
        maybeTeamHigh: None,
        maybeTeamLow: None,
        maybeTeamJack: None,
      }, effects)

    | PlayCard(playerId, c) =>
      switch(state.phase){
      | PlayerTurnPhase(phasePlayerId) when phasePlayerId == playerId =>
        let player = Quad.get(playerId, state.players);
        let hand' = List.filter(c' => c != c', player.pla_hand);
        let cardMaybesOnBoard = state.players->Quad.map(player => player.pla_card, _);
        switch(state.maybeTrumpCard){
        | None => 
          // This should be an impossible state.
          // No player should be able to play a card when there is no trump on board.
          logger.warn("Player is somehow playing a card when trump is None");
          ( state, effects )
        | Some({suit: trumpSuit}) =>
          let validationResult =
          ValidatePlay.validate(
            state.phase,
            state.leader,
            state.game_follow_suit,
            player.pla_hand,
            playerId,
            c,
            cardMaybesOnBoard,
            state.maybeLeadCard->Belt.Option.map(leadCard => leadCard.suit),
            trumpSuit,
          );

          let updateGame = () => {
            /*
              When the current player is the last player in the trick (i.e. the next player
              is the lead player), it means this current player will end the trick. There
              is no need to advance the turn since The true next player will be determined
              later by computing the trick winner. This test keeps the ui more consistent
              if the player who wins the trick is the last player in the trick. I can't keep
              the game in the PlayerTurnPhase since the client needs to know its in a State
              where the player is not allowed to trigger any events until the timer transitions 
              the game into another active phase.
              */
            let nextPlayer = Quad.nextId(playerId);
            let phase' = nextPlayer == state.leader ? IdlePhase(None, UpdateGameIdle) : PlayerTurnPhase(nextPlayer);
            let nextPlayers =
              Quad.update(playerId, x => {...x, pla_hand: hand', pla_card: Some(c)}, state.players);

            let maybeGetTeamJackAward =
                ((maybeCard1, maybeCard2, maybeCard3, maybeCard4), maybeLeadCard, trumpSuit) => {
              switch (
                My.Option.all5(maybeCard1, maybeCard2, maybeCard3, maybeCard4, maybeLeadCard)
              ) {
              | None => None;
              | Some((card1, card2, card3, card4, leadCard)) =>
                let trick = (card1, card2, card3, card4);
                let jackOfTrump = Card.{rank: Card.Rank.Jack, suit: trumpSuit};
                let (trickWinnerId, _card) = Trick.getWinnerCard(trumpSuit, leadCard.Card.suit, (card1, card2, card3, card4));
                let trickWinnerTeamId = Game.teamOfPlayer(trickWinnerId);
                switch (Quad.withId(trick) |> Quad.getWhere(((_playerId, card)) => card == jackOfTrump)) {
                | None => None;
                | Some((playerId, _card)) =>
                  let jackHolderTeamId = Game.teamOfPlayer(playerId);
                  jackHolderTeamId == trickWinnerTeamId
                    ? Some({GameAward.team_id: jackHolderTeamId, jack_award_type: GameAward.RunJackAward})
                    : Some({team_id: trickWinnerTeamId, jack_award_type: HangJackAward});
                };
              };
            };

            let (maybeTeamJackAward, jackAwardNotis) =
              switch (state.maybeTeamJack) {
              | None =>
                // name-all-the-things iffy for sets of maybe-items
                let iffyTrick = Quad.map(player => player.pla_card, nextPlayers);
                let maybeTeamJackAward =
                  maybeGetTeamJackAward(iffyTrick, state.maybeLeadCard, trumpSuit);
                let jackAwardNotis =
                  switch (maybeTeamJackAward) {
                  | None => []
                  | Some({jack_award_type}) =>
                    switch (jack_award_type) {
                    | RunJackAward => Noti.broadcast(~msg=Text("Jack gets away!"), ())
                    | HangJackAward => Noti.broadcast(~msg=Text("Jack gets hanged!"), ())
                    }
                  };
                (maybeTeamJackAward, jackAwardNotis);
              | Some(teamJackAward) => (Some(teamJackAward), [])
              };
            
            let notiEffects = jackAwardNotis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

            ({
              ...state,
              players: nextPlayers,
              maybeLeadCard: Js.Option.isNone(state.maybeLeadCard) ? Some(c) : state.maybeLeadCard,
              maybeTeamJack: maybeTeamJackAward,
              phase: phase',
            }, effects @ notiEffects);
          };

          switch (validationResult) {
          | Belt.Result.Error(validationError) =>
            let noti_message =
              switch (validationError) {
              | CardNotInHand => Noti.Text("How are you even playing that card?")
              | CantUnderTrump => Text("You can't under-trump. Play a higher trump or another suit.")
              | MustFollowSuit => 
                switch(My.Option.all2(state.maybeLeadCard, state.maybeTrumpCard)){
                | None => Text("You can't play that card.")
                | Some(({suit: leadSuit}, {suit: trumpSuit})) =>
                  Text({j|You must follow suit ($leadSuit) or play trump ($trumpSuit).|j})
                }
              | MustFollowTrumpedSuit =>
                switch (My.Option.all2(state.maybeTrumpCard, state.game_follow_suit)) {
                | None => Text("You can't play that card.")
                | Some(({suit: trumpSuit}, suitToFollow)) =>
                  let trumpSuitText = trumpSuit->Card.Suit.toString;
                  let suitToFollowText = suitToFollow->Card.Suit.toString;
                  Text({j|You must follow with the suit you trumped on ($suitToFollowText) or play trump ($trumpSuitText).|j});
                }
              | WaitForTurn => Text("Wait for your turn.")
              | AlreadyPlayed => Text("You already have a card in play.")
              };

            let errorNoti = {
              Noti.noti_id: Nanoid.nanoid(),
              noti_recipient: playerId,
              noti_message,
              noti_level: Danger,
              noti_kind: Duration(3750),
            };

            let errorNotiEffect = ServerEvent.NotifyPlayer(game_key, errorNoti);

            (state, [errorNotiEffect, ...effects]);

          | Belt.Result.Ok () => updateGame()
          };
        };

      | _ => 
        let playerIdText = playerId->Player.stringOfId
        logger.warn(
          {j|`PlayerTurnPhase($playerIdText)` recieved out of phase.|j}
        );
        (state, effects);
      }

    | Deal =>
      switch(state.phase){
      | DealPhase => 
        let dealCards = state => {
          let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, state.deck);
          let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck);
          let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck);
          let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck);
          {
            ...state,
            deck,
            leader: Quad.nextId(state.dealer),
            players:
              state.players
              |> Quad.update(N1, x => {...x, pla_hand: p1Hand})
              |> Quad.update(N2, x => {...x, pla_hand: p2Hand})
              |> Quad.update(N3, x => {...x, pla_hand: p3Hand})
              |> Quad.update(N4, x => {...x, pla_hand: p4Hand}),
            teams: GameTeams.map(x => {...x, team_points: 0}, state.teams),
          };
        };

        let kickTrump = state => {
          let dealerTeam = teamOfPlayer(state.dealer);
          let (cards, deck) = Deck.deal(1, state.deck);
          let trumpCard = List.hd(cards); /* Dealing expects enough cards to kick trump. #unsafe */
          let points = kickPoints(trumpCard.rank);

          {
            ...state,
            deck,
            maybeTrumpCard: Some(trumpCard),
            teams:
              GameTeams.update(dealerTeam, x => {...x, team_score: x.team_score + points}, state.teams),
          };
        };

        let state = state |> dealCards |> kickTrump;

        
        let kickTrumpNotiEffects =
          getKickTrumpNotis(state.maybeTrumpCard)
          ->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

        ({
          ...state,
          phase: isGameOverTest(state) ? GameOverPhase(Quad.make(_ => RematchUnknown)) : BegPhase,
        }, kickTrumpNotiEffects @ effects);

      | _ =>
        logger.warn("`Deal` recieved out of phase.");
        ( state, effects )
      };

    | AdvanceRound => 
      let (p1CardMaybe, p2CardMaybe, p3CardMaybe, p4CardMaybe) =
        state.players->Quad.map(p => p.pla_card, _);

      switch (
        My.Option.all6(
          p1CardMaybe,
          p2CardMaybe,
          p3CardMaybe,
          p4CardMaybe,
          state.maybeLeadCard,
          state.maybeTrumpCard,
        )
      ) {
      | None => ( state, effects )
      | Some((p1Card, p2Card, p3Card, p4Card, {Card.suit: leadSuit}, {Card.suit: trumpSuit})) =>
        let trick = (p1Card, p2Card, p3Card, p4Card);

        let (trickWinner, trickWinnerCard) = Trick.getWinnerCard(trumpSuit, leadSuit, trick);

        let game_follow_suit =
          switch (state.game_follow_suit) {
          // clear the follow-suit field whenever trick-winner is not the leader
          | Some(_) when state.leader != trickWinner => None
          // the above case clears the follow-suit field in all cases when the trick-winner 
          // is not the leader. The only remaining case when game_follow_suit is Some(thing) 
          // would be when the trick-winner *is* the leader and in this case I only None the follow-suit
          // flag if the leader did indeed follow with the suit-to-follow 
          | Some(suitToFollow) when trickWinnerCard.suit == suitToFollow => None
          | game_followsuit_maybe =>
            // in order for trump-and-follow to be but in effect
            // the leading-suit *must not* be trump
            leadSuit != trumpSuit
            // the player must win the trick with trump
            && trickWinnerCard.suit == trumpSuit
            // and must still have a leading-suit card in hand
            && Quad.select(
                trickWinner,
                p => (p.pla_hand->List.exists(({Card.suit}) => suit == leadSuit, _)),
                state.players,
              )
              ? Some(leadSuit) : game_followsuit_maybe;
          };

        // Commented out the notifications because it might get annoying for people 
        // who already accustomed to trump-and-follow.
        // let followSuitNotis =
        //   game_follow_suit->Belt.Option.mapWithDefault(
        //     [],
        //     capturedSuit => {
        //       let trumpSuitText = trumpSuit->Card.Suit.toString;
        //       let capturedSuitText = capturedSuit->Card.Suit.toString;
        //       [
        //         {
        //           Noti.noti_id: Nanoid.nanoid(),
        //           noti_recipient: trickWinner,
        //           noti_message:
        //             Text(
        //               {j|You must follow with the captured suit ($capturedSuitText) or play trump ($trumpSuitText)|j},
        //             ),
        //           noti_level: Warning,
        //           noti_kind: Confirm,
        //         },
        //       ];
        //     },
        //   );
        let advanceRound = state => {
          ...state,
          players:
            Quad.update(trickWinner, x => {...x, pla_tricks: x.pla_tricks @ [trick]}, state.players)
            |> Quad.map(x => {...x, pla_card: None}),
          teams:
            GameTeams.update(
              teamOfPlayer(trickWinner),
              x => {...x, team_points: x.team_points + Trick.getValue(trick)},
              state.teams,
            ),
          leader: trickWinner,
          maybeLeadCard: None,
          phase: PlayerTurnPhase(trickWinner),
          game_follow_suit,
          // notis: state.notis @ followSuitNotis
        };

        let state = state |> advanceRound;

        let (gameOverTestState, (h, l, j, g)) =
          Util.updateUntil(
            [maybeAddHighPoint, maybeAddLowPoint, maybeAddJackPoints],
            ((state, _)) => isGameOverTest(state),
            (state, (None, None, None, None)),
          );

        if (isGameOverTest(gameOverTestState)) {
          let notis =
            Noti.broadcast(
              ~msg=
                RoundSummary({
                  noti_maybeTeamHigh: h,
                  noti_maybeTeamLow: l,
                  noti_maybeTeamJack: j,
                  noti_maybeTeamGame: g,
                }),
              ~kind=Confirm,
              (),
            );
          let notiEffects = notis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
          ( {...gameOverTestState
            , phase: GameOverPhase(Quad.make(_ => RematchUnknown))
            }
          , notiEffects @ effects
          );
        } else {
          /* Any player whose hand is empty at this points indicates all players' hands are empty */
          List.length(Quad.get(N1, state.players).pla_hand) == 0
            ? reduceRec(NewRound, state, effects) : (state, effects);
        };
      };
    | Beg =>
      switch (state.phase) {
      | BegPhase =>
        let beggerId = Quad.nextId(state.dealer);
        switch (state.clients->Quad.get(beggerId, _)) {
        | Vacant
        | Disconnected(_, _) => ( state, effects )
        | Connected(client) =>
          let notis =
            Noti.playerBroadcast(
              ~from=beggerId,
              ~msg=Noti.Text(client.client_username ++ " begs"),
              (),
            );
          let notiEffects = notis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
          ( {...state
            , phase: GiveOnePhase
            }
          ,  notiEffects @ effects 
          );
        };
      | _ =>
        logger.warn("`Beg` recieved out of phase.");
        (state, effects)
      };


    | Stand =>
      switch(state.phase){
      | BegPhase =>
        let beggerId = Quad.nextId(state.dealer);

        switch (Quad.get(beggerId, state.clients)) {
        | Vacant
        | Disconnected(_) => ( state, effects )
        | Connected(begger) =>
          let (maybeTeamHigh, maybeTeamLow) =
            getTeamHighAndLowMaybes(
              state.players->Quad.map(player => player.pla_hand, _),
              state.maybeTrumpCard,
            );

          let notiEffects =
            Noti.playerBroadcast(~from=beggerId, ~msg=Noti.Text(begger.client_username ++ " stands"), ())
            ->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

          let effects = notiEffects @ effects;
          let state' = {
            ...state,
            maybeTeamHigh,
            maybeTeamLow,
          };

          let (gameOverTestState, (h, l, j, g)) =
            Util.updateUntil(
              [maybeAddHighPoint, maybeAddLowPoint],
              ((state, _)) => isGameOverTest(state),
              (state', (None, None, None, None)),
            );

          if (isGameOverTest(gameOverTestState)) {
            let notis =
              Noti.broadcast(
                ~msg=
                  RoundSummary({
                    noti_maybeTeamHigh: h,
                    noti_maybeTeamLow: l,
                    noti_maybeTeamJack: j,
                    noti_maybeTeamGame: g,
                  }),
                ~kind=Confirm,
                (),
              );
            
            let notiEffects = notis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
            ({
              ...gameOverTestState,
              phase: GameOverPhase(Quad.make(_ => RematchUnknown)),
            }, notiEffects @ effects);
          } else {
            ( { ...state' // don't use gameOverTestState here. Points for high and low should usually be added at the end of the round.
              , phase: PlayerTurnPhase(beggerId),
              }
            , effects
            )
          };

        };
      | _ =>
        logger.warn("`Stand` recieved out of phase.");
        ( state, effects );
      };


    // | GiveOne
    //     when
    //       [Quad.N1, N3]
    //       |> List.mem(state.dealer)
    //       && GameTeams.get(T2, state.teams).team_score == 13
    //       || [Quad.N2, N4]
    //       |> List.mem(state.dealer)
    //       && GameTeams.get(T1, state.teams).team_score == 13 =>
    //   state;
    | GiveOne =>
      switch(state.phase){
      | GiveOnePhase =>
        switch (Quad.get(state.dealer, state.clients)) {
        | Vacant
        | Disconnected(_) => ( state, effects )
        | Connected(dealer) =>
          let receivingTeamId =
            switch (state.dealer) {
            | N1 | N3 => Team.T2
            | N2 | N4 => T1
            };

          let (maybeTeamHigh, maybeTeamLow) =
            getTeamHighAndLowMaybes(
              state.players->Quad.map(player => player.pla_hand, _),
              state.maybeTrumpCard,
            );

          let giveOneNotiEffects = Noti.playerBroadcast(
                  ~from=state.dealer,
                  ~msg=Noti.Text(dealer.client_username ++ " gives one."),
                  (),
                )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

          let state' = {
            ...state,
            teams:
              GameTeams.update(receivingTeamId, x => {...x, team_score: x.team_score + 1}, state.teams),
            maybeTeamHigh,
            maybeTeamLow,
          };

          let (gameOverTestState, (h, l, j, g)) =
            Util.updateUntil(
              [maybeAddHighPoint, maybeAddLowPoint],
              ((state, _)) => isGameOverTest(state),
              (state', (None, None, None, None)),
            );

          if (isGameOverTest(gameOverTestState)) {
            let roundSummaryNotiEffects =
              Noti.broadcast(
                ~msg=
                  RoundSummary({
                    noti_maybeTeamHigh: h,
                    noti_maybeTeamLow: l,
                    noti_maybeTeamJack: j,
                    noti_maybeTeamGame: g,
                  }),
                ~kind=Confirm,
                (),
              )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
            (
              {...gameOverTestState, phase: GameOverPhase(Quad.make(_ => RematchUnknown))},
              roundSummaryNotiEffects @ effects,
            );
          } else {
            ({...state', phase: PlayerTurnPhase(state.leader)}, giveOneNotiEffects @ effects);
          };
        };
      | _ =>
        logger.warn("`GiveOne` recieved out of phase.");
        ( state, effects );
      };

    | RunPack =>
      switch(state.phase){
      | GiveOnePhase
      | RunPackPhase =>
        let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, state.deck);
        let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
        let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
        let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);

        let client_username = state.clients->Game.getUsername(state.dealer);

        let prevKick =
          switch (state.maybeTrumpCard) {
          | None =>
            failwith(
              "DealMore action expected state.maybeTrumpCard to be Some thing but got None",
            )
          | Some(k) => k
          };

        let kick = Js.Option.getExn(state.maybeTrumpCard);
        let {Card.suit: kickSuit} = kick;

        let (cards, deck) = Deck.deal(1, deck);
        let kick' = List.hd(cards);
        let {Card.rank: kickRank', Card.suit: kickSuit'} = kick';
        let pointsKicked = kickPoints(kickRank');

        let state =
          kickSuit == kickSuit'
            ? {
              ...state,
              phase:
                List.length(deck) < SharedGame.settings.nCardsToRun * 4 + 1 // enough for 4 players and kicking 1 trump?
                  ? PackDepletedPhase : RunPackPhase,
            }
            : {
              ...state,
              phase: PlayerTurnPhase(state.leader),
            };
        
        let runPackNotiEffects = Noti.playerBroadcast(
              ~from=state.dealer,
              ~msg=Noti.Text(client_username ++ " runs the pack"),
              (),
            )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

        let state = {
          ...state,
          players:
            state.players
            |> Quad.update(N1, x => {...x, pla_hand: x.pla_hand @ p1Hand})
            |> Quad.update(N2, x => {...x, pla_hand: x.pla_hand @ p2Hand})
            |> Quad.update(N3, x => {...x, pla_hand: x.pla_hand @ p3Hand})
            |> Quad.update(N4, x => {...x, pla_hand: x.pla_hand @ p4Hand}),
          deck: deck @ [prevKick],
          maybeTrumpCard: Some(kick'),
          teams:
            GameTeams.update(
              teamOfPlayer(state.dealer),
              x => {...x, team_score: x.team_score + pointsKicked},
              state.teams,
            ),
        };

        let (maybeTeamHigh, maybeTeamLow) =
          getTeamHighAndLowMaybes(
            state.players->Quad.map(player => player.pla_hand, _),
            state.maybeTrumpCard,
          );

        let kickTrumpNotiEffects =
          getKickTrumpNotis(state.maybeTrumpCard)
          ->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

        let state = 
          {...state
          , maybeTeamHigh
          , maybeTeamLow
          };

        if (isGameOverTest(state)) {
          ( {...state
            , phase: GameOverPhase(Quad.make(_ => RematchUnknown))
            }
          , kickTrumpNotiEffects @ runPackNotiEffects @ effects);
        } else {
          switch (state.phase) {
          | PlayerTurnPhase(_) =>
            let (gameOverTestState, (h, l, j, g)) =
              Util.updateUntil(
                [maybeAddHighPoint, maybeAddLowPoint],
                ((state, _)) => isGameOverTest(state),
                (state, (None, None, None, None)),
              );

            if (isGameOverTest(gameOverTestState)) {
              let roundSummaryNotiEffects =
                Noti.broadcast(
                  ~msg=
                    RoundSummary({
                      noti_maybeTeamHigh: h,
                      noti_maybeTeamLow: l,
                      noti_maybeTeamJack: j,
                      noti_maybeTeamGame: g,
                    }),
                  ~kind=Confirm,
                  (),
                )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

              ( {...gameOverTestState
                , phase: GameOverPhase(Quad.make(_ => RematchUnknown))
                }
              , roundSummaryNotiEffects @ effects);
            } else {
              ( state, effects );
            };
          | _ => ( state, effects )
          };
        };

      | _ =>
        logger.warn("`RunPack` recieved out of phase.");
        ( state, effects )
      };

    | DealAgain =>
      switch(state.phase){
      | PackDepletedPhase => 
        let client_username = state.clients->getUsername(state.dealer);
        let redealNotiEffects = Noti.playerBroadcast(
              ~from=state.dealer,
              ~msg=Noti.Text(client_username ++ " has to redeal"),
              (),
            )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
        ({
          ...state,
          players: Quad.map(x => {...x, pla_hand: []}, state.players),
          maybeTrumpCard: None,
          deck: Deck.make() |> Deck.shuffle,
          phase: DealPhase,
        }, redealNotiEffects @ effects)
      | _ => 
        logger.warn("`DealAgain` recieved out of phase.");
        ( state, effects )
      };

    | LeaveGame(leavingPlayerId) => 
      switch(state.clients->Quad.get(leavingPlayerId, _)){
      | Vacant =>
        logger.warn("Ignoring `LeaveGame` recieved for already Vacant seat.");
        ( state, effects)
      | Connected(client)
      | Disconnected(client, _) =>
        let getNextPhase = (nPlayersToFind, currentPhase) => {
          switch (currentPhase) {
          | FindSubsPhase({phase: subPhase}) => FindSubsPhase({ emptySeatCount: nPlayersToFind, phase: subPhase })
          | FindPlayersPhase({ canSub }) => FindPlayersPhase({ emptySeatCount: nPlayersToFind, canSub })
          | GameOverPhase(rematchDecisions) =>
            let rematchDecisions = rematchDecisions->Quad.put(leavingPlayerId, RematchDenied, _);
            let numRematchUnknowns =
              rematchDecisions->Quad.countHaving(_, decision => decision == RematchUnknown);
            if (numRematchUnknowns == 0) {
              FindPlayersPhase({ emptySeatCount: nPlayersToFind, canSub: false });
            } else {
              GameOverPhase(rematchDecisions);
            };

          | IdlePhase(Some(timeout), StartGameIdle) =>
            Timer.clearTimeout(timeout);
            FindPlayersPhase({emptySeatCount: nPlayersToFind, canSub: false})
          | IdlePhase(Some(timeout), reason) =>
            FindSubsPhase({ emptySeatCount: nPlayersToFind, phase: IdlePhase(Some(timeout->Timer.pauseTimeout), reason) })
          | phase => FindSubsPhase({ emptySeatCount: nPlayersToFind, phase })
          };
        };
        
        let clients = state.clients->Quad.update(leavingPlayerId, _ => Vacant, _);

        let playerLeftNotiEffects =
          Noti.playerBroadcast(
            ~from=leavingPlayerId,
            ~msg=Noti.Text(client.client_username ++ " has left game."),
            ~level=Warning,
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));

        
        /* 
        * When the leaving player was the game master and the game has at least
        * one other seat till taken, choose a new game master from one of the
        * taken seats.
        */
        let game_id =
          switch (state.game_id) {
          | Private({private_game_key, private_game_host}) when private_game_host == leavingPlayerId =>
            switch(clients->Quad.withId->Quad.find(((_quadId, client)) => client->Game.isSeatTaken)){
            /* None means no taken seats were found => no players in the game => this game will be discarded by the ServerStore 
              So I just leave the id as it is. */
            | None => state.game_id 
            | Some((idOfTakenSeat, _client)) => Private({private_game_key, private_game_host: idOfTakenSeat})
            }
          | game_id => game_id
          };
        
        ( {
          ...state,
          game_id,
          clients,
          phase: getNextPhase(clients->Quad.countHaving(clientState => clientState == Vacant), state.phase),
          maybeKickTimeoutId: None /* This timeout should be cleared by the code issuing the LeaveGame action */
        }, playerLeftNotiEffects @ effects);
      }

    | UpdateSubbing(canSub) => 
      let phase =
        switch (state.phase) {
        | FindPlayersPhase({ emptySeatCount }) => FindPlayersPhase({ emptySeatCount, canSub })
        | phase => phase
        };
      ( {...state, phase}, effects );
    | StartGame => ( {...state, phase: DealPhase}, effects );
    | SkipIdling => 
      let state = switch (state.phase) {
      | IdlePhase(Some(timeout), idleReason) =>
        switch(timeout){
        | RunningTimeout(_, task, _, _) => 
          Timer.clearTimeout(timeout);
          let timeout = Timer.startTimeout(task, 0);
          {
            ...state,
            phase: IdlePhase(Some(timeout), idleReason),
          }
        | _ => state
        }
      | _ => state
      };
      (state, effects)
    | PrivateToPublic => 
      let state = switch(state.game_id){
      | Public(_) => state
      | Private({private_game_key: key}) => {...state, game_id: Public(key)}
      };
      (state, effects)
    | Transition({fromPhase, toPhase}) =>
      ({
        ...state, 
        phase: state.phase == fromPhase ? toPhase : state.phase,
      },effects)
    
    | AttachClient(seat_id, clientState) =>
      switch(clientState){
      | Vacant => (state, effects)
      | Connected({client_username})
      | Disconnected({client_username}, _) =>
        let clients = state.clients->Quad.put(seat_id, clientState, _);
        let playersNeeded = clients->Quad.countHaving(clientState => clientState == Vacant); 
        let phase =
          switch (state.phase) {
            /* Suppose a player left during the delay before advancing the round, then this new player joins
                I need to put the game back into the idling phase with the delay restarted. */
          | FindSubsPhase({ phase: IdlePhase(Some(timeout), idleReason) }) when playersNeeded == 0 =>
              Game.IdlePhase(Some(Timer.restartTimeout(timeout)), idleReason)
          // | FindSubsPhase({phase: subPhase}) when playersNeeded == 0 =>
          //   let timeout = Timer.startTimeout(() => update(ResumeGame(sock_id))
          | FindSubsPhase({phase: subPhase}) =>
            FindSubsPhase({ emptySeatCount: playersNeeded, phase: subPhase })
          | FindPlayersPhase({  canSub }) => FindPlayersPhase({ emptySeatCount:playersNeeded, canSub })
          | otherPhase => otherPhase
          };
        
        let playerJoinedNotiEffects =
          Noti.playerBroadcast(
            ~from=seat_id,
            ~msg=Noti.Text(client_username ++ " joined the game."),
            ~level=Noti.Success,
            (),
          )
          ->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti));
        
        let effectsNext =
          switch (phase) {
          | FindPlayersPhase({emptySeatCount: 0}) => [
              ServerEvent.CreateGameTimer(game_key, TransitionGameCountdown(phase, DealPhase)),
              ...playerJoinedNotiEffects,
            ]
          | FindSubsPhase({emptySeatCount: 0, phase: subPhase}) => [
              ServerEvent.CreateGameTimer(game_key, TransitionGameCountdown(phase, subPhase)),
              ...playerJoinedNotiEffects,
            ]
          | _ => playerJoinedNotiEffects
          };
        
        ({...state, clients, phase}, effects @ effectsNext);
      }
      
    };
  };

  reduceRec(action, state, [] )
  |> addLeaveStateEffects(state)
  |> addEnterStateEffects(state);
}
