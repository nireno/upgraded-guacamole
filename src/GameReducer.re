open AppPrelude;
open Game;

let rec reducer = (action, state) =>
      switch (action) {
      | Noop => state
      | NewRound =>
        let maybeAddPoints = (maybeTeam, points, state) =>
          switch (maybeTeam) {
          | None => state
          | Some(team) => addPoints(team, points, state)
          };

        let maybeAddJackPoints = (maybeTeamJack, state) =>
          switch (maybeTeamJack) {
          | None => state
          | Some((team, award)) =>
            addPoints(team, valueOfAward(award), state)
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
          p1Tricks: [],
          p2Tricks: [],
          p3Tricks: [],
          p4Tricks: [],
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
        let hand = Game.getPlayerHand(player, state);
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

        if(!canPlayCard){
          state;
        } else {
          let state = Game.updateHand(player, hand', state);
          let state =
            switch (state.maybeLeadCard) {
            | None => {...state, maybeLeadCard: Some(c)}
            | _ => state
            };

          let state = {...state, board: state.board @ [c]};
          let state =
            switch (state.maybePlayerTurn) {
            | None => state
            | Some(turn) => {...state, maybePlayerTurn: Some(Player.nextPlayer(turn))}
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
          {...state, deck, p1Hand, p2Hand, p3Hand, p4Hand};
        };

        let kickTrump = state => {
          let dealerTeam = teamOfPlayer(state.dealer);
          let (cards, deck) = Deck.deal(1, state.deck);
          let trumpCard = List.hd(cards); /* Dealing expects enough cards to kick trump. #unsafe */
          let points = kickPoints(trumpCard.rank);

          {...state, deck, maybeTrumpCard: Some(trumpCard)}
          |> addPoints(dealerTeam, points);
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
        let (p1Index, p2Index, p3Index, p4Index) =
          playerBoardIndices(state.leader);
        let {Card.suit: leadSuit} = Js.Option.getExn(state.maybeLeadCard); /* Action requires leadCard. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard); /* Action requires trumpCard. #unsafe */

        let trick =
          Trick.{
            p1Card: List.nth(state.board, p1Index),
            p2Card: List.nth(state.board, p2Index),
            p3Card: List.nth(state.board, p3Index),
            p4Card: List.nth(state.board, p4Index),
          }; /* Action requires that the board has four cards. #unsafe*/

        let trickWinner: Player.id =
          Trick.playerTakesTrick(trumpSuit, leadSuit, trick);

        let collectTrick = (player, trick, state) =>
          switch (player) {
          | Player.P1 => {...state, p1Tricks: state.p1Tricks @ [trick]}
          | P2 => {...state, p2Tricks: state.p2Tricks @ [trick]}
          | P3 => {...state, p3Tricks: state.p3Tricks @ [trick]}
          | P4 => {...state, p4Tricks: state.p4Tricks @ [trick]}
          };

        let advanceRound = state => {
          ...state,
          leader: trickWinner,
          maybePlayerTurn: Some(trickWinner),
          maybeLeadCard: None,
          board: [],
        };

        let state = state  |> collectTrick(trickWinner, trick) |> advanceRound;
        List.length(state.p1Hand) == 0
          ? reducer(EndRound, state) : state;
      | EndRound =>
        /* @endround start */
        Js.log("@EndRound");
        let team1Tricks = state.p1Tricks @ state.p3Tricks;
        let team2Tricks = state.p2Tricks @ state.p4Tricks;
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
            && state.team2Points == 13
            || [Player.P2, P4]
            |> List.mem(state.dealer)
            && state.team1Points == 13 =>
        Js.log("Your opponents only need one point!");
        state;
      | GiveOne =>
        Js.log("Take one");
        let state =
          switch (state.dealer) {
          | P1
          | P3 => {...state, team2Points: state.team2Points + 1}
          | P2
          | P4 => {...state, team1Points: state.team1Points + 1}
          };
        {
          ...state,
          phase: PlayerTurnPhase,
          maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
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
        let points = kickPoints(kickRank');
        let state =
          switch (teamOfPlayer(state.dealer)) {
          | Team.T1 => {...state, team1Points: state.team1Points + points}
          | Team.T2 => {...state, team2Points: state.team2Points + points}
          };
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
          p1Hand: state.p1Hand @ p1Hand,
          p2Hand: state.p2Hand @ p2Hand,
          p3Hand: state.p3Hand @ p3Hand,
          p4Hand: state.p4Hand @ p4Hand,
          deck: deck @ [prevKick],
          maybeTrumpCard: Some(kick'),
        };
        
        let updateStatePhase = state => 
          {...state, phase: isGameOverTest(state) ? GameOverPhase : state.phase};

        state |> updateStatePhase;
      | DealAgain =>
        {
          ...state,
          p1Hand: [],
          p2Hand: [],
          p3Hand: [],
          p4Hand: [],
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
        let phasemod = game => {
          switch (game.phase) {
          | FindSubsPhase(_n, subPhase) => {...game, phase: FindSubsPhase(4 - Game.playerCount(game), subPhase)}
          | FindPlayersPhase(_n) => {...game, phase: FindPlayersPhase(4 - Game.playerCount(game))}
          | GameOverPhase => game
          | phase => {...game, phase: FindSubsPhase(4 - Game.playerCount(game), phase)}
          };
        };

        state 
        |> Game.removePlayerSocket(playerId) 
        |> Game.removePlayerName(playerId) 
        |> phasemod;

      | CheatPoints(team, value) =>
        addPoints(team, value, state)
      }
