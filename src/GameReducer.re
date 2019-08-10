open Game;

type action =
  | Noop
  | PlayCard(Player.id, Card.t)
  | EndTrick
  | AdvanceRound
  | NewRound
  | Beg
  | Stand
  | GiveOne
  | Deal
  | RunPack
  | DealAgain
  | LeaveGame(Player.id)
  | UpdateSubbing(bool)
  | ClearNotis;

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
      (game_phase, playerId, game_leader_id, cardPlayed, pla_hand, pla_card, game_followsuit_maybe, game_trumpcard_maybe) => {
    game_phase != PlayerTurnPhase(playerId)
      ? Belt.Result.Error(WaitForTurn)
      : pla_card != None
          ? Belt.Result.Error(AlreadyPlayed)
          : 
            !List.exists(card => card == cardPlayed, pla_hand)
              ? Belt.Result.Error(CardNotInHand)
              // there must be an empty slot on the board for the player
              : (
                switch (game_followsuit_maybe) {
                | Some(suitToFollow)
                    when
                      game_leader_id == playerId
                      && game_trumpcard_maybe->Belt.Option.mapWithDefault(false, trumpCard =>
                           trumpCard.Card.suit != cardPlayed.Card.suit
                         )
                      && cardPlayed.suit != suitToFollow =>
                  Belt.Result.Error(MustFollowTrumpedSuit)
                | _ => Ok()
                }
              );
  };
};

/* 
  This module should probably be called GameMachine and this function should
  be the gameState machine. It takes some (before) state and an action and
  produces some (after) state state.
*/
let rec reduce = (action, state) =>
  switch (action) {
  | Noop => state
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

    {...state, 
      phase: isGameOverTest(state) ? GameOverPhase : DealPhase,
      deck: Deck.make() |> Deck.shuffle,
      players: Quad.map(x => {...x, pla_tricks: []}, state.players),
      dealer: nextDealer,
      maybeTrumpCard: None,
      maybeLeadCard: None,
      maybeTeamHigh: None,
      maybeTeamLow: None,
      maybeTeamJack: None,
      notis: Noti.(broadcast(~msg=RoundSummary({
        noti_maybeTeamHigh: h,
        noti_maybeTeamLow: l,
        noti_maybeTeamJack: j,
        noti_maybeTeamGame: g
      } ), ~kind=Confirm, ()))
    }

  | PlayCard(playerId, c) =>
    let player = Quad.get(playerId, state.players);
    let hand' = List.filter(c' => c != c', player.pla_hand);

    let validationResult =
      ValidatePlay.validate(
        state.phase,
        playerId,
        state.leader,
        c,
        player.pla_hand,
        player.pla_card,
        state.game_follow_suit,
        state.maybeTrumpCard,
      );

    let updateGame = () => {
      /*
        When the current player is the last player in the trick (i.e. the next player
        is the lead player), it means this current player will end the trick. There
        is no need to advance the turn since The true next player will be determined
        later by computing the trick winner. This test keeps the ui more consistent
        if the player who wins the trick is the last player in the trick.
        */
      let nextPlayer = Quad.nextId(playerId);
      let phase' = nextPlayer == state.leader ? IdlePhase : PlayerTurnPhase(nextPlayer);
      let nextPlayers =
        Quad.update(playerId, x => {...x, pla_hand: hand', pla_card: Some(c)}, state.players);

      let maybeGetTeamJackAward =
          ((maybeCard1, maybeCard2, maybeCard3, maybeCard4), maybeTrumpCard, maybeLeadCard) => {
        switch (
          My.Option.all6(maybeCard1, maybeCard2, maybeCard3, maybeCard4, maybeTrumpCard, maybeLeadCard)
        ) {
        | None => None;
        | Some((card1, card2, card3, card4, trumpCard, leadCard)) =>
          let trick = (card1, card2, card3, card4);
          let jackOfTrump = Card.{rank: Card.Rank.Jack, suit: trumpCard.suit};
          let (trickWinnerId, _card) = Trick.getWinnerCard(trumpCard.Card.suit, leadCard.Card.suit, (card1, card2, card3, card4));
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
            maybeGetTeamJackAward(iffyTrick, state.maybeTrumpCard, state.maybeLeadCard);
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


      {
        ...state,
        players: nextPlayers,
        maybeLeadCard: Js.Option.isNone(state.maybeLeadCard) ? Some(c) : state.maybeLeadCard,
        maybeTeamJack: maybeTeamJackAward,
        phase: phase',
        notis: jackAwardNotis,
      };
    };

    switch (validationResult) {
    | Belt.Result.Error(validationError) =>
      let noti_message =
        switch (validationError) {
        | CardNotInHand => Noti.Text("How are you even playing that card?")
        | CantUnderTrump => Text("You can't under-trump.")
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
            Text({j|You must follow with your captured suit ($suitToFollowText) or play trump ($trumpSuitText).|j});
          }
        | WaitForTurn => Text("Wait for your turn.")
        | AlreadyPlayed => Text("You already have a card in play.")
        };

      let errorNoti = {
        Noti.noti_id: Nanoid.nanoid(),
        noti_recipient: playerId,
        noti_message,
        noti_level: Danger,
        noti_kind: Duration(5000),
      };

      {...state, notis: state.notis @ [errorNoti]};

    | Belt.Result.Ok () => updateGame()
    };

  | Deal =>
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

    let kickTrumpNotis = getKickTrumpNotis(state.maybeTrumpCard);

    {
      ...state,
      phase: isGameOverTest(state) ? GameOverPhase : BegPhase,
      notis: state.notis @ kickTrumpNotis,
    };

  | EndTrick => state
    // TODO: Endtrick is practically a noop at this point. 
    // It is probably unecessary and can be removed.
    // let updatePlayers = state => {...state, maybePlayerTurn: None};
    // state |> updatePlayers;

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
    | None => state
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
        {...gameOverTestState, phase: GameOverPhase, notis: gameOverTestState.notis @ notis};
      } else {
        /* Any player whose hand is empty at this points indicates all players' hands are empty */
        List.length(Quad.get(N1, state.players).pla_hand) == 0
          ? reduce(NewRound, state) : state;
      };
    };
  | Beg =>
    let beggerId = Quad.nextId(state.dealer);
    let pla_name = Quad.get(beggerId, state.players).pla_name;
    let notis = Noti.playerBroadcast(~from=beggerId, ~msg=Noti.Text(pla_name ++ " begs"), ());

    {...state, phase: GiveOnePhase, notis};

  | Stand =>
    let beggerId = Quad.nextId(state.dealer);
    let begger = Quad.get(beggerId, state.players);

    let (maybeTeamHigh, maybeTeamLow) =
      getTeamHighAndLowMaybes(
        state.players->Quad.map(player => player.pla_hand, _),
        state.maybeTrumpCard,
      );

    let state' = {
      ...state,
      maybeTeamHigh,
      maybeTeamLow,
      notis: Noti.playerBroadcast(~from=beggerId, ~msg=Noti.Text(begger.pla_name ++ " stands"), ()),
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

      {...gameOverTestState, phase: GameOverPhase, notis: gameOverTestState.notis @ notis};
    } else {
      {
        ...state', // don't use gameOverTestState here. Points for high and low should usually be added at the end of the round.
        phase: PlayerTurnPhase(beggerId),
      };
    };
    

  | GiveOne
      when
        [Quad.N1, N3]
        |> List.mem(state.dealer)
        && GameTeams.get(T2, state.teams).team_score == 13
        || [Quad.N2, N4]
        |> List.mem(state.dealer)
        && GameTeams.get(T1, state.teams).team_score == 13 =>
    state;
  | GiveOne =>
    let receivingTeamId =
      switch (state.dealer) {
      | N1 | N3 => Team.T2 
      | N2 | N4 => T1
      };

    let dealer = Quad.get(state.dealer, state.players);

    let (maybeTeamHigh, maybeTeamLow) =
      getTeamHighAndLowMaybes(
        state.players->Quad.map(player => player.pla_hand, _),
        state.maybeTrumpCard,
      );

    let state' = {
      ...state,
      teams:
        GameTeams.update(receivingTeamId, x => {...x, team_score: x.team_score + 1}, state.teams),
      notis:
        state.notis
        @ Noti.playerBroadcast(
            ~from=state.dealer,
            ~msg=Noti.Text(dealer.pla_name ++ " gives one."),
            (),
          ),
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
      {...gameOverTestState, phase: GameOverPhase, notis: gameOverTestState.notis @ notis};
    } else {
      {...state', phase: PlayerTurnPhase(state.leader)};
    };

  | RunPack =>
    let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, state.deck);
    let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
    let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
    let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
    let dealer = Quad.get(state.dealer, state.players);

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
            List.length(deck) < 12 ? PackDepletedPhase : RunPackPhase,
        }
        : {
          ...state,
          phase: PlayerTurnPhase(state.leader),
        };

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
      notis:
        Noti.playerBroadcast(
          ~from=state.dealer,
          ~msg=Noti.Text(dealer.pla_name ++ " runs the pack"),
          (),
        ),
    };

    let (maybeTeamHigh, maybeTeamLow) =
      getTeamHighAndLowMaybes(
        state.players->Quad.map(player => player.pla_hand, _),
        state.maybeTrumpCard,
      );

    let kickTrumpNotis = getKickTrumpNotis(state.maybeTrumpCard);

    let state = {...state, maybeTeamHigh, maybeTeamLow, notis: state.notis @ kickTrumpNotis};

    if (isGameOverTest(state)) {
      {...state, phase: GameOverPhase};
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

          {...gameOverTestState, phase: GameOverPhase, notis: gameOverTestState.notis @ notis};
        } else {
          state;
        };
      | _ => state
      };
    };

  | DealAgain =>
    let dealer = Quad.get(state.dealer, state.players);
    {
      ...state,
      players: Quad.map(x => {...x, pla_hand: []}, state.players),
      maybeTrumpCard: None,
      deck: Deck.make() |> Deck.shuffle,
      phase: DealPhase,
      notis:
        Noti.playerBroadcast(
          ~from=state.dealer,
          ~msg=Noti.Text(dealer.pla_name ++ " has to redeal"),
          (),
        ),
    }

  | LeaveGame(leavingPlayerId) => 
    let modPhase = (nPlayers, currentPhase) =>
      switch (currentPhase) {
      | FindSubsPhase(_n, subPhase) => FindSubsPhase(nPlayers, subPhase)
      | FindPlayersPhase(_n, canSub) => FindPlayersPhase(nPlayers, canSub)
      | GameOverPhase => GameOverPhase
      | phase => FindSubsPhase(nPlayers, phase)
      };
    
    let leavingPlayer = Quad.get(leavingPlayerId, state.players);

    let players =
      state.players
      |> Quad.update(leavingPlayerId, x =>
            {...x, pla_name: Player.stringOfId(leavingPlayerId), sock_id_maybe: None}
          );

    let playerLeftNotis =
      Noti.playerBroadcast(
        ~from=leavingPlayerId,
        ~msg=Noti.Text(leavingPlayer.pla_name ++ " has left game."),
        ~level=Warning,
        (),
      );

    {
      ...state,
      players: players,
      phase: state.phase |> modPhase(4 - countPlayers(players)),
      notis: state.notis @ playerLeftNotis,
      maybeKickTimeoutId: None /* This timeout should be cleared by the code issuing the LeaveGame action */
    };
  | UpdateSubbing(canSub) => 
    let phase =
      switch (state.phase) {
      | FindPlayersPhase(n, _) => FindPlayersPhase(n, canSub)
      | phase => phase
      };
    {...state, phase};
  | ClearNotis => 
    {...state, notis: []};
  };
