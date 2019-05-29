open AppPrelude;
open Game;

let rec reducer = (action, state) =>
      switch (action) {
      | Noop => state
      | NewRound =>
        let maybeAddPoints = (maybeTeam, points, state) =>
          switch (maybeTeam) {
          | None => state
          | Some(teamId) => {
              ...state,
              teams:
                GameTeams.update(teamId, x => {...x, team_score: x.team_score + points}, state.teams),
            }
          };

        let maybeAddJackPoints = (maybeTeamJack, state) =>
          switch (maybeTeamJack) {
          | None => state
          | Some((team, award)) => {
              ...state,
              teams:
                GameTeams.update(
                  team,
                  x => {...x, team_score: x.team_score + valueOfAward(award)},
                  state.teams,
                ),
            }
          };

        let updateScore = state =>
          Util.updateUntil(
            [
              maybeAddPoints(state.maybeTeamHigh, valueOfAward(HighAward)),
              maybeAddPoints(state.maybeTeamLow, valueOfAward(LowAward)),
              maybeAddJackPoints(state.maybeTeamJack),
              maybeAddPoints(state.maybeTeamGame, valueOfAward(GameAward)),
            ],
            isGameOverTest,
            state,
          );

        let updateBoard = state => {
          ...state,
          deck: Deck.make() |> Deck.shuffle,
          players: Quad.map(x => {...x, pla_tricks: []}, state.players),
          teams: GameTeams.map(x => {...x, team_points: 0}, state.teams),
          maybeTrumpCard: None,
          maybeLeadCard: None,
          maybePlayerTurn: None,
          maybeTeamHigh: None,
          maybeTeamLow: None,
          maybeTeamJack: None,
          maybeTeamGame: None,
        };

        let updatePlayers = state => {
          let nextDealer = Player.nextPlayer(state.dealer);
          let nextLeader = Player.nextPlayer(nextDealer);
          {...state, dealer: nextDealer, leader: nextLeader};
        };

        let updatePhase = state => 
          {...state, phase: isGameOverTest(state) ? GameOverPhase : DealPhase};

        state |> updateScore |> updateBoard |> updatePlayers 
              |> updatePhase

      | PlayCard(player, c) =>
        let hand = GamePlayers.select(player, x => x.pla_hand, state.players);
        let hand' = List.filter(c' => c != c', hand);

        let canPlayCard = 
        switch(state.maybePlayerTurn){
          | None => false
          | Some(playerId) when playerId != player => false
          | Some(_) => 
            hand == hand' 
              ? false  // Card c was not in player hand
              : true
        }

        if (!canPlayCard) {
          state;
        } else {
          let state = {
            ...state,
            players: GamePlayers.update(
                player,
                x => {...x, pla_hand: hand', pla_card: Some(c)},
                state.players,
              ),
            maybeLeadCard: Js.Option.isNone(state.maybeLeadCard) ? Some(c) : state.maybeLeadCard,
            maybePlayerTurn:
              switch (state.maybePlayerTurn) {
              | None => state.maybePlayerTurn
              | Some(turn) => Some(Player.nextPlayer(turn))
              },
          };

          Player.nextPlayer(player) == state.leader 
            ? reducer(EndTrick, state) 
            : state;
        };
      | Deal =>
        let dealCards = state => {
          let (p1Hand, deck) = Deck.deal(6, state.deck);
          let (p2Hand, deck) = Deck.deal(6, deck);
          let (p3Hand, deck) = Deck.deal(6, deck);
          let (p4Hand, deck) = Deck.deal(6, deck);
          {
            ...state,
            deck,
            players:
              state.players
              |> GamePlayers.update(Player.P1, x => {...x, pla_hand: p1Hand})
              |> GamePlayers.update(Player.P2, x => {...x, pla_hand: p2Hand})
              |> GamePlayers.update(Player.P3, x => {...x, pla_hand: p3Hand})
              |> GamePlayers.update(Player.P4, x => {...x, pla_hand: p4Hand}),
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

        {
          ...state,
          phase: isGameOverTest(state) ? GameOverPhase : BegPhase,
        };

      | EndTrick =>
        let updatePlayers = state => {...state, maybePlayerTurn: None};
        state |> updatePlayers;

      | AdvanceRound => 
        let {Card.suit: leadSuit} = Js.Option.getExn(state.maybeLeadCard); /* Action requires leadCard. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard); /* Action requires trumpCard. #unsafe */

        let getPlayerCard = playerId =>
          GamePlayers.select(playerId, x => Js.Option.getExn(x.pla_card), state.players); /* #unsafe */

        let trick = 
          Trick.{
            p1Card: getPlayerCard(P1),
            p2Card: getPlayerCard(P2),            
            p3Card: getPlayerCard(P3),
            p4Card: getPlayerCard(P4),
          }; /* Action requires that the board has four cards. #unsafe*/

        let trickWinner: Player.id =
          Trick.playerTakesTrick(trumpSuit, leadSuit, trick);

        let advanceRound = state => {
          ...state,
          players:
            GamePlayers.update(
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
          maybePlayerTurn: Some(trickWinner),
          maybeLeadCard: None,
        };

        let state = state  |> advanceRound;

        /* Any player whose hand is empty at this points indicates all players' hands are empty */
        List.length(GamePlayers.get(P1, state.players).pla_hand) == 0
          ? reducer(EndRound, state) : state;
      | EndRound =>
        /* @endround start */
        Js.log("@EndRound");

        let team1Tricks =
          GamePlayers.get(P1, state.players).pla_tricks 
          @ GamePlayers.get(P3, state.players).pla_tricks;
        let team2Tricks =
          GamePlayers.get(P2, state.players).pla_tricks 
          @ GamePlayers.get(P4, state.players).pla_tricks;

        let playersCards: list((Player.id, Card.t)) =
          team1Tricks
          @ team2Tricks
          |> List.map(trickToPlayerCards)
          |> List.concat;

        /** Action requires that the game has a trump card kicked. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard);

        let playersTrumpAsc: list((Player.id, Card.t)) =
          playersCards
          |> List.filter(((_, {Card.suit})) => suit == trumpSuit)
          |> List.sort(((_, {Card.rank: rank1}), (_, {rank: rank2})) =>
               compare(rank1, rank2)
             );

        let playersTrumDesc = List.rev(playersTrumpAsc);

        let lowPlayerCard =
          try (Some(List.hd(playersTrumpAsc))) {
          | Failure("hd") => None
          };

        let highPlayerCard =
          try (Some(List.hd(playersTrumDesc))) {
          | Failure("hd") => None
          };

        let jackPlayerCard =
          try (
            Some(
              playersCards
              |> List.filter(((_player, {Card.rank, suit})) =>
                   rank == Card.Rank.Jack && suit == trumpSuit
                 )
              |> List.hd,
            )
          ) {
          | Failure("hd") => None
          };

        let isPlayerJackHangedTest: ((Player.id, Card.t)) => bool = (
          playerJack => {
            let (player, jack) = playerJack;
            let playerTeam = teamOfPlayer(player);
            let team1TrickedJack =
              List.exists(trickContainsCard(jack), team1Tricks);
            switch (playerTeam) {
            | T1 => team1TrickedJack ? false : true
            | T2 => team1TrickedJack ? true : false
            };
          }
        );

        let state =
          switch (jackPlayerCard) {
          | None => {...state, maybeTeamJack: None}
          | Some(playerJack) =>
            let (player, _jack) = playerJack;
            let isJackHanged = isPlayerJackHangedTest(playerJack);
            switch (teamOfPlayer(player)) {
            | T1 =>
              isJackHanged
                ? {...state, maybeTeamJack: Some((T2, HangJackAward))}
                : {...state, maybeTeamJack: Some((T1, RunJackAward))}
            | T2 =>
              isJackHanged
                ? {...state, maybeTeamJack: Some((T1, HangJackAward))}
                : {...state, maybeTeamJack: Some((T2, RunJackAward))}
            };
          };

        let state =
          switch (lowPlayerCard) {
          | None => {...state, maybeTeamLow: None}
          | Some((player, _card)) => {
              ...state,
              maybeTeamLow: Some(teamOfPlayer(player)),
            }
          };

        let state =
          switch (highPlayerCard) {
          | None => {...state, maybeTeamHigh: None}
          | Some((player, _card)) => {
              ...state,
              maybeTeamHigh: Some(teamOfPlayer(player)),
            }
          };

        let calcPoints = tricks =>
          tricks
          |> List.map(Trick.cardsInTrick)
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
            : team1Points > team2Points ? Some(Team.T1) : Some(Team.T2);
        {...state, maybeTeamGame, phase: RoundSummaryPhase};
        /* @endround end */
      | Beg =>
        Js.log("Ah beg");
        {...state, phase: GiveOnePhase};
      | Stand =>
        {
          ...state,
          maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
          phase: PlayerTurnPhase,
        }
      | GiveOne
          when
            [Player.P1, P3]
            |> List.mem(state.dealer)
            && GameTeams.get(T2, state.teams).team_score == 13
            || [Player.P2, P4]
            |> List.mem(state.dealer)
            && GameTeams.get(T1, state.teams).team_score == 13 =>
        state;
      | GiveOne =>
        let receivingTeamId =
          switch (state.dealer) {
          | P1 | P3 => Team.T2 
          | P2 | P4 => T1
          };

        {
          ...state,
          phase: PlayerTurnPhase,
          maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
          teams:
            GameTeams.update(receivingTeamId, x => {...x, team_score: x.team_score + 1}, state.teams),
        };

      | RunPack =>
        Js.log("I beg too");
        let (p1Hand, deck) = Deck.deal(3, state.deck);
        let (p2Hand, deck) = Deck.deal(3, deck);
        let (p3Hand, deck) = Deck.deal(3, deck);
        let (p4Hand, deck) = Deck.deal(3, deck);
        let prevKick =
          switch (state.maybeTrumpCard) {
          | None =>
            failwith(
              "DealMore action expected state.kick to be Some thing but got None",
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
              phase: PlayerTurnPhase,
              maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
            };

        let state = {
          ...state,
          players:
            state.players
            |> GamePlayers.update(P1, x => {...x, pla_hand: x.pla_hand @ p1Hand})
            |> GamePlayers.update(P2, x => {...x, pla_hand: x.pla_hand @ p2Hand})
            |> GamePlayers.update(P3, x => {...x, pla_hand: x.pla_hand @ p3Hand})
            |> GamePlayers.update(P4, x => {...x, pla_hand: x.pla_hand @ p4Hand}),
          deck: deck @ [prevKick],
          maybeTrumpCard: Some(kick'),
          teams:
            GameTeams.update(
              teamOfPlayer(state.dealer),
              x => {...x, team_score: x.team_score + pointsKicked},
              state.teams,
            ),
        };
        
        let updateStatePhase = state => 
          {...state, phase: isGameOverTest(state) ? GameOverPhase : state.phase};

        state |> updateStatePhase;
      | DealAgain =>
        {
          ...state,
          players: Quad.map(x => {...x, pla_hand: []}, state.players),
          maybeTrumpCard: None,
          deck: Deck.make() |> Deck.shuffle,
          phase: DealPhase,
        }
      | BlockPlay(player) =>
        switch (player) {
        | P1 => Js.log("Not your turn p1")
        | P2 => Js.log("Not your turn p2")
        | P3 => Js.log("Not your turn p3")
        | P4 => Js.log("Not your turn p4")
        };

        state;

      | LeaveGame(playerId) => 
        let modPhase = (nPlayers, currentPhase) =>
          switch (currentPhase) {
          | FindSubsPhase(_n, subPhase) => FindSubsPhase(nPlayers, subPhase)
          | FindPlayersPhase(_n) => FindPlayersPhase(nPlayers)
          | GameOverPhase => GameOverPhase
          | phase => FindSubsPhase(nPlayers, phase)
          };

        let players =
          state.players
          |> GamePlayers.update(playerId, x =>
               {...x, pla_name: Player.stringOfId(playerId), pla_socket: None}
             );

        {
          ...state,
          players: players,
          phase: state.phase |> modPhase(4 - Game.countPlayers(players)),
        };

      | CheatPoints(team, points) =>
        {
          ...state,
          teams: GameTeams.update(team, x => {...x, team_score: x.team_score + points}, state.teams),
        };
      }
