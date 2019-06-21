open AppPrelude;
open Game;

let rec reducer = (action, state) =>
      switch (action) {
      | Noop => state
      | NewRound =>
        /* what used to be EndRound start */
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

        let playersTrumpDesc = List.rev(playersTrumpAsc);

        let lowPlayerCard =
          try (Some(List.hd(playersTrumpAsc))) {
          | Failure("hd") => None
          };

        let highPlayerCard =
          try (Some(List.hd(playersTrumpDesc))) {
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
        // {...state, maybeTeamGame};
        /* what used to be EndRound end */

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
                  x => {...x, team_score: x.team_score + GameAward.value(award)},
                  state.teams,
                ),
            }
          };

        let updateScore = state =>
          Util.updateUntil(
            [
              maybeAddPoints(state.maybeTeamHigh, GameAward.value(HighAward)),
              maybeAddPoints(state.maybeTeamLow, GameAward.value(LowAward)),
              maybeAddJackPoints(state.maybeTeamJack),
              maybeAddPoints(maybeTeamGame, GameAward.value(GameAward)),
            ],
            isGameOverTest,
            state,
          );


        let nextDealer = Player.nextPlayer(state.dealer);

        let state = updateScore(state);

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
          maybeTeamGame: None,
          notis: Noti.(broadcast(~msg=RoundSummary({
            noti_maybeTeamHigh: state.maybeTeamHigh,
            noti_maybeTeamLow: state.maybeTeamLow,
            noti_maybeTeamJack: state.maybeTeamJack,
            noti_maybeTeamGame: maybeTeamGame
          } ), ~kind=Confirm, ()))
        }
              
      | PlayCard(playerId, c) =>
        let player = GamePlayers.get(playerId, state.players);
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
          let nextPlayer = Player.nextPlayer(playerId);
          let phase' = nextPlayer == state.leader ? IdlePhase : PlayerTurnPhase(nextPlayer);

          {
            ...state,
            players:
              GamePlayers.update(
                playerId,
                x => {...x, pla_hand: hand', pla_card: Some(c)},
                state.players,
              ),
            maybeLeadCard: Js.Option.isNone(state.maybeLeadCard) ? Some(c) : state.maybeLeadCard,
            phase: phase',
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
            leader: Player.nextPlayer(state.dealer),
            players:
              state.players
              |> GamePlayers.update(Player.P1, x => {...x, pla_hand: p1Hand})
              |> GamePlayers.update(Player.P2, x => {...x, pla_hand: p2Hand})
              |> GamePlayers.update(Player.P3, x => {...x, pla_hand: p3Hand})
              |> GamePlayers.update(Player.P4, x => {...x, pla_hand: p4Hand}),
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

        {
          ...state,
          phase: isGameOverTest(state) ? GameOverPhase : BegPhase,
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
          maybeLeadCard: None,
          phase: PlayerTurnPhase(trickWinner),
        };

        let state = state  |> advanceRound;

        /* Any player whose hand is empty at this points indicates all players' hands are empty */
        List.length(GamePlayers.get(P1, state.players).pla_hand) == 0
          ? reducer(NewRound, state) : state;
      | Beg =>
        let beggerId = Player.nextPlayer(state.dealer);
        let pla_name = GamePlayers.get(beggerId, state.players).pla_name;
        let notis = Noti.playerBroadcast(~from=beggerId, ~msg=Noti.Text(pla_name ++ " begs"), ());

        {...state, phase: GiveOnePhase, notis};
      | Stand =>
        let beggerId = Player.nextPlayer(state.dealer);
        let begger = GamePlayers.get(beggerId, state.players);
        {
          ...state,
          phase: PlayerTurnPhase(beggerId),
          notis: Noti.playerBroadcast(~from=beggerId, ~msg=Noti.Text(begger.pla_name ++ " stands"), ())
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
        
        let dealer = GamePlayers.get(state.dealer, state.players);

        {
          ...state,
          phase: PlayerTurnPhase(state.leader),
          teams:
            GameTeams.update(receivingTeamId, x => {...x, team_score: x.team_score + 1}, state.teams),
          notis: state.notis @ Noti.playerBroadcast(~from=state.dealer, ~msg=Noti.Text(dealer.pla_name ++ " gives one."), ())
        };

      | RunPack =>
        Js.log("I beg too");
        let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, state.deck);
        let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
        let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
        let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck);
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
              phase: PlayerTurnPhase(state.leader),
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

      | LeaveGame(leavingPlayerId) => 
        let modPhase = (nPlayers, currentPhase) =>
          switch (currentPhase) {
          | FindSubsPhase(_n, subPhase) => FindSubsPhase(nPlayers, subPhase)
          | FindPlayersPhase(_n) => FindPlayersPhase(nPlayers)
          | GameOverPhase => GameOverPhase
          | phase => FindSubsPhase(nPlayers, phase)
          };
        
        let leavingPlayer = GamePlayers.get(leavingPlayerId, state.players);

        let players =
          state.players
          |> GamePlayers.update(leavingPlayerId, x =>
               {...x, pla_name: Player.stringOfId(leavingPlayerId), pla_socket: None}
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
          phase: state.phase |> modPhase(4 - Game.countPlayers(players)),
          notis: state.notis @ playerLeftNotis
        };
      | ClearNotis => 
        {...state, notis: []}
      | CheatPoints(team, points) =>
        {
          ...state,
          teams: GameTeams.update(team, x => {...x, team_score: x.team_score + points}, state.teams),
        };
      }
