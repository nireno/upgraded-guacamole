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
  | ClearNotis
  | CheatPoints(Team.id, int);

let getTeamHighAndLowMaybes:
  ((Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t), option(Card.t)) =>
  (option((Team.id, Card.t)), option(( Team.id, Card.t ))) =
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

      let lowPlayerCard =
        try (Some(List.hd(playersTrumpAsc))) {
        | Failure("hd") => None
        };

      let maybeTeamLow =
        switch (lowPlayerCard) {
        | None => None
        | Some((playerId, card)) => Some((playerId->teamOfPlayer, card))
        };

      let highPlayerCard =
        try (Some(List.hd(playersTrumpDesc))) {
        | Failure("hd") => None
        };

      let maybeTeamHigh =
        switch (highPlayerCard) {
        | None => None
        | Some((playerId, card)) => Some((playerId->teamOfPlayer, card))
        };

      (maybeTeamHigh, maybeTeamLow);
    };
  };


let maybeAddHighPoint = state =>
  switch (state.maybeTeamHigh) {
  | None => state
  | Some((teamId, _card)) => {
      ...state,
      teams:
        GameTeams.update(
          teamId,
          x => {...x, team_score: x.team_score + GameAward.value(HighAward)},
          state.teams,
        ),
    }
  };

let maybeAddLowPoint = state =>
  switch (state.maybeTeamLow) {
  | None => state
  | Some((teamId, _card)) => {
      ...state,
      teams:
        GameTeams.update(
          teamId,
          x => {...x, team_score: x.team_score + GameAward.value(LowAward)},
          state.teams,
        ),
    }
  };

let maybeAddJackPoints = state =>
  switch (state.maybeTeamJack) {
  | None => state
  | Some((team, award)) => {
      ...state,
      teams:
        GameTeams.update(
          team,
          x => {...x, team_score: x.team_score + GameAward.jackAwardValue(award)},
          state.teams,
        ),
    }
  };

let getWinningTeamMaybe = ({teams: ({team_score: team1Score}, {team_score: team2Score})}) =>
  team1Score == SharedGame.settings.winningScore
    ? Some(Team.T1) : team2Score == SharedGame.settings.winningScore ? Some(T2) : None;

let getHLJ = (game, teamId) => {
  let h =
    switch (game.maybeTeamHigh) {
    | Some((teamHighId, card)) when teamHighId == teamId => Some(card)
    | _ => None
    };

  let l =
    switch (game.maybeTeamLow) {
    | Some((teamLowId, card)) when teamLowId == teamId => Some(card)
    | _ => None
    };

  let j =
    switch (game.maybeTeamJack) {
    | Some((teamJackId, jackAward)) when teamJackId == teamId => Some(jackAward)
    | _ => None
    };

  (h, l, j);
};

let getDecisiveAward = hlj =>
  switch (hlj) {
  | (None, None, None) => None
  | (Some(card), None, None) => Some(GameAward.HighDecides(card))
  | (None, Some(card), None) => Some(LowDecides(card))
  | (None, None, Some(jackAward)) =>
    switch (jackAward) {
    | GameAward.RunJackAward => Some(RunJackDecides)
    | HangJackAward => Some(HangJackDecides)
    }
  | (Some(highCard), Some(lowCard), None) => Some(HighAndLowDecides(highCard, lowCard))
  | (Some(highCard), None, Some(jackAward)) =>
    switch (jackAward) {
    | RunJackAward => Some(HighAndRunJackDecides(highCard))
    | HangJackAward => Some(HighAndHangJackDecides(highCard))
    }
  | (None, Some(lowCard), Some(jackAward)) =>
    switch (jackAward) {
    | RunJackAward => Some(LowAndRunJackDecides(lowCard))
    | HangJackAward => Some(LowAndHangJackDecides(lowCard))
    }
  | (Some(highCard), Some(lowCard), Some(jackAward)) =>
    switch (jackAward) {
    | RunJackAward => Some(HighLowAndRunJackDecides(highCard, lowCard))
    | HangJackAward => Some(HighLowAndHangJackDecides(highCard, lowCard))
    }
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
            ? None
            : team1Points > team2Points ? Some((Team.T1, team1Points, team2Points)) : Some((Team.T2, team2Points, team1Points));

        let maybeAddGamePoint = (maybeTeamGame, state) =>
          switch (maybeTeamGame) {
          | None => state
          | Some((teamId, _, _)) => {
              ...state,
              teams:
                GameTeams.update(
                  teamId,
                  x => {...x, team_score: x.team_score + GameAward.value(GameAward)},
                  state.teams,
                ),
            }
          };

        let updateScore = state =>
          Util.updateUntil(
            [
              maybeAddHighPoint,
              maybeAddLowPoint,
              maybeAddJackPoints,
              maybeAddGamePoint(maybeTeamGame),
            ],
            isGameOverTest,
            state,
          );

        let nextDealer = Quad.nextId(state.dealer);

        let state = updateScore(state);

        {...state, 
          phase: isGameOverTest(state) ? GameOverPhase(None) : DealPhase,
          deck: Deck.make() |> Deck.shuffle,
          players: Quad.map(x => {...x, pla_tricks: []}, state.players),
          dealer: nextDealer,
          maybeTrumpCard: None,
          maybeLeadCard: None,
          maybeTeamHigh: None,
          maybeTeamLow: None,
          maybeTeamJack: None,
          notis: Noti.(broadcast(~msg=RoundSummary({
            noti_maybeTeamHigh: state.maybeTeamHigh,
            noti_maybeTeamLow: state.maybeTeamLow,
            noti_maybeTeamJack: state.maybeTeamJack,
            noti_maybeTeamGame: maybeTeamGame
          } ), ~kind=Confirm, ()))
        }

      | PlayCard(playerId, c) =>
        let player = Quad.get(playerId, state.players);
        let hand' = List.filter(c' => c != c', player.pla_hand);

        let canPlayCard =
          // This player has the turn
          state.phase == PlayerTurnPhase(playerId)
          // and Card c was in player hand
          && player.pla_hand != hand'
          // and there is an empty slot on the board for the player
          && player.pla_card == None
            ? true : false;

        if (!canPlayCard) {
          state;
        } else {
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
                  ? Some((jackHolderTeamId, GameAward.RunJackAward))
                  : Some((trickWinnerTeamId, HangJackAward));
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
                | Some((_teamId, award)) =>
                  switch (award) {
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

        let maybeDecisiveAward = switch(state.maybeTrumpCard){
        | None => None
        | Some(card) => Some(GameAward.KickDecides(card))
        };

        {
          ...state,
          phase: isGameOverTest(state) ? GameOverPhase(maybeDecisiveAward) : BegPhase,
        };

      | EndTrick => state
        // TODO: Endtrick is practically a noop at this point. 
        // It is probably unecessary and can be removed.
        // let updatePlayers = state => {...state, maybePlayerTurn: None};
        // state |> updatePlayers;

      | AdvanceRound => 
        let {Card.suit: leadSuit} = Js.Option.getExn(state.maybeLeadCard); /* Action requires leadCard. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard); /* Action requires trumpCard. #unsafe */

        let getPlayerCard = playerId =>
          Quad.select(playerId, x => Js.Option.getExn(x.pla_card), state.players); /* #unsafe */

        /* This action requires that the board has four cards. #unsafe*/
        let trick = (getPlayerCard(N1), getPlayerCard(N2), getPlayerCard(N3), getPlayerCard(N4));

        let (trickWinner, _card) =
          Trick.getWinnerCard(trumpSuit, leadSuit, trick);

        let advanceRound = state => {
          ...state,
          players:
            Quad.update(
              trickWinner,
              x => {...x, pla_tricks: x.pla_tricks @ [trick]},
              state.players,
            ) |> Quad.map(x => {...x, pla_card: None}),
          teams:
            GameTeams.update(
              teamOfPlayer(trickWinner),
              x => {...x, team_points: x.team_points + Trick.getValue(trick)},
              state.teams,
            ),
          leader: trickWinner,
          maybeLeadCard: None,
          phase: PlayerTurnPhase(trickWinner),
        };

        let state = state |> advanceRound;

        let gameOverTestState =
          Util.updateUntil(
            [maybeAddHighPoint, maybeAddLowPoint, maybeAddJackPoints],
            isGameOverTest,
            state,
          );

        gameOverTestState
        ->getWinningTeamMaybe
        ->Belt.Option.mapWithDefault(
          /* Any player whose hand is empty at this points indicates all players' hands are empty */
            List.length(Quad.get(N1, state.players).pla_hand) == 0
            ? reduce(NewRound, state) : state,
            teamId => {
              let decisiveAward = gameOverTestState->getHLJ(teamId)->getDecisiveAward;
              {...gameOverTestState, phase: GameOverPhase(decisiveAward)};
            },
          );
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

        let gameOverTestState =
          Util.updateUntil([maybeAddHighPoint, maybeAddLowPoint], isGameOverTest, state');

        switch (gameOverTestState->getWinningTeamMaybe) {
        | None => {
            ...state', // don't use gameOverTestState here. Points for high and low should usually be added at the end of the round.
            phase: PlayerTurnPhase(beggerId),
          }

        | Some(teamId) =>
          let decisiveAward = gameOverTestState->getHLJ(teamId)->getDecisiveAward;
          {...gameOverTestState, phase: GameOverPhase(decisiveAward)};
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

        let gameOverTestState =
          Util.updateUntil([maybeAddHighPoint, maybeAddLowPoint], isGameOverTest, state');

        switch (gameOverTestState->getWinningTeamMaybe) {
        | None => {...state', phase: PlayerTurnPhase(state.leader)}

        | Some(teamId) =>
          let decisiveAward = gameOverTestState->getHLJ(teamId)->getDecisiveAward;
          {...gameOverTestState, phase: GameOverPhase(decisiveAward)};
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

        let state = {...state, maybeTeamHigh, maybeTeamLow};

        let maybeDecisiveAward =
          switch (state.maybeTrumpCard) {
          | None => None
          | Some(card) => Some(GameAward.KickDecides(card))
          };

        if (isGameOverTest(state)) {
          {...state, phase: GameOverPhase(maybeDecisiveAward)};
        } else {
          switch (state.phase) {
          | PlayerTurnPhase(_) =>
            let gameOverTestState =
              Util.updateUntil([maybeAddHighPoint, maybeAddLowPoint], isGameOverTest, state);

            gameOverTestState
            ->getWinningTeamMaybe
            ->Belt.Option.mapWithDefault(
                state,
                teamId => {
                  let decisiveAward = gameOverTestState->getHLJ(teamId)->getDecisiveAward;
                  {...gameOverTestState, phase: GameOverPhase(decisiveAward)};
                },
              );
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
          | GameOverPhase(x) => GameOverPhase(x)
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
        {...state, notis: []}
      | CheatPoints(team, points) =>
        {
          ...state,
          teams: GameTeams.update(team, x => {...x, team_score: x.team_score + points}, state.teams),
        };
      }
