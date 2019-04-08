type maybePlayerTurn = option(Player.id);

type pointValue =
  | OnePoint
  | ThreePoints;

let pointValueToInt =
  fun
  | OnePoint => 1
  | ThreePoints => 3;

type phase =
  | RoundSummary
  | Game;

let isPlayerTurn = (turn, playerId) => {
  switch (turn) {
  | None => false
  | Some(turn) => turn == playerId
  };
};

let teamOfPlayer =
  Player.(
    fun
    | P1
    | P3 => Team.T1
    | P2
    | P4 => Team.T2
  );

/** The board is just a list of cards. It doesn't track which player played them.
But if I know which player is the leader, I can tell which card belongs to which player */
let playerBoardIndices = leader => {
  Player.(
    switch (leader) {
    | P1 => (0, 1, 2, 3)
    | P2 => (3, 0, 1, 2)
    | P3 => (2, 3, 0, 1)
    | P4 => (1, 2, 3, 0)
    }
  );
};

let handToPlayerCards: (Player.id, Hand.t) => list((Player.id, Card.t)) =
  (player, hand) => {
    List.map(card => (player, card), hand);
  };

let trickToPlayerCards: Trick.t => list((Player.id, Card.t)) =
  trick => {
    [
      (P1, trick.p1Card),
      (P2, trick.p2Card),
      (P3, trick.p3Card),
      (P4, trick.p4Card),
    ];
  };

let tricksToPlayerCards: list(Trick.t) => list((Player.id, Card.t)) =
  tricks => {
    tricks |> List.map(trickToPlayerCards) |> List.concat;
  };

let trickContainsCard: (Card.t, Trick.t) => bool =
  (testCard, trick) => {
    trickToPlayerCards(trick)
    |> List.exists(((_, card)) => card == testCard);
  };

let stringOfTeam =
  fun
  | Team.T1 => "Team 1"
  | T2 => "Team 2";

module App = {
  type action =
    | PlayCard(Player.id, Player.hand, Card.t)
    | BlockPlay(Player.id)
    | EndTrick
    | NewRound
    | EndRound
    | Beg
    | Stand
    | GiveOne
    | DealMore
    | Deal;

  type state = {
    deck: Deck.t,
    board: list(Card.t),
    p1Hand: Player.hand,
    p2Hand: Player.hand,
    p3Hand: Player.hand,
    p4Hand: Player.hand,
    p1Tricks: list(Trick.t),
    p2Tricks: list(Trick.t),
    p3Tricks: list(Trick.t),
    p4Tricks: list(Trick.t),
    maybeTrumpCard: option(Card.t), /* using slot suffix to denote an optional prop. */
    maybeLeadCard: option(Card.t),
    me: Player.id,
    dealer: Player.id,
    leader: Player.id,
    maybePlayerTurn,
    team1Points: int,
    team2Points: int,
    canBeg: bool,
    canStand: bool,
    canDeal: bool,
    canDealMore: bool,
    canGiveOne: bool,
    teamHigh: option(Team.id),
    teamLow: option(Team.id),
    teamJack: option((Team.id, pointValue)),
    teamGame: Team.id,
    phase,
  };

  let component = ReasonReact.reducerComponent("AllFoursApp");

  let make = _children => {
    ...component,
    initialState: () => {
      {
        me: P1,
        deck: Deck.make() |> Deck.shuffle,
        board: [],
        p1Hand: [],
        p2Hand: [],
        p3Hand: [],
        p4Hand: [],
        p1Tricks: [],
        p2Tricks: [],
        p3Tricks: [],
        p4Tricks: [],
        maybePlayerTurn: None,
        maybeTrumpCard: None,
        maybeLeadCard: None,
        dealer: P1,
        leader: Player.nextPlayer(P1),
        team1Points: 0,
        team2Points: 0,
        canBeg: false,
        canStand: false,
        canDeal: true,
        canDealMore: false,
        canGiveOne: false,
        teamHigh: None,
        teamLow: None,
        teamJack: None,
        teamGame: T1,
        phase: Game,
      };
    },
    reducer: (action, state) =>
      switch (action) {
      | NewRound =>
        let addPoints = (team, value, state) => {
          switch (team) {
          | Team.T1 => {
              ...state,
              team1Points: state.team1Points + pointValueToInt(value),
            }
          | T2 => {
              ...state,
              team2Points: state.team2Points + pointValueToInt(value),
            }
          };
        };

        let maybeAddPoints = (maybeTeam, points, state) =>
          switch (maybeTeam) {
          | None => state
          | Some(team) => addPoints(team, points, state)
          };

        let maybeAddJackPoints = (maybeTeamJack, state) =>
          switch (maybeTeamJack) {
          | None => state
          | Some((team, value)) => addPoints(team, value, state)
          };

        let resetBoard = state => {
          let dealer = Player.nextPlayer(state.dealer);
          let leader = Player.nextPlayer(dealer);
          {
            ...state,
            deck: Deck.make() |> Deck.shuffle,
            p1Tricks: [],
            p2Tricks: [],
            p3Tricks: [],
            p4Tricks: [],
            maybeTrumpCard: None,
            maybeLeadCard: None,
            dealer,
            leader,
            maybePlayerTurn: None,
            canDeal: true,
            phase: Game,
          };
        };

        let state =
          state
          |> maybeAddPoints(state.teamHigh, OnePoint)
          |> maybeAddPoints(state.teamLow, OnePoint)
          |> maybeAddJackPoints(state.teamJack)
          |> addPoints(state.teamGame, OnePoint)
          |> resetBoard;

        ReasonReact.Update(state);
      | PlayCard(player, hand, c) =>
        let hand' = List.filter(c' => c != c', hand);
        let state =
          switch (player) {
          | P1 => {...state, p1Hand: hand'}
          | P2 => {...state, p2Hand: hand'}
          | P3 => {...state, p3Hand: hand'}
          | P4 => {...state, p4Hand: hand'}
          };

        let state =
          switch (state.maybeLeadCard) {
          | None => {...state, maybeLeadCard: Some(c)}
          | _ => state
          };

        Js.log(
          Player.toString(player) ++ " played: " ++ Card.stringOfCard(c),
        );

        let state = {...state, board: state.board @ [c]};
        let state =
          switch (state.maybePlayerTurn) {
          | None => state
          | Some(turn) => {
              ...state,
              maybePlayerTurn: Some(Player.nextPlayer(turn)),
            }
          };

        Js.log(
          "Player is: "
          ++ Player.toString(player)
          ++ "; Leader is: "
          ++ Player.toString(state.leader),
        );

        Player.nextPlayer(player) == state.leader
          ? ReasonReact.UpdateWithSideEffects(
              state,
              ({send}) => send(EndTrick),
            )
          : ReasonReact.Update(state);

      | Deal =>
        let (p1Hand, deck) = Deck.deal(2, state.deck);
        let (p2Hand, deck) = Deck.deal(2, deck);
        let (p3Hand, deck) = Deck.deal(2, deck);
        let (p4Hand, deck) = Deck.deal(2, deck);
        let state = {...state, p1Hand, p2Hand, p3Hand, p4Hand};

        let (cards, deck) = Deck.deal(1, deck);
        let kick = List.hd(cards);
        let updateKickPoints = (kick, dealerTeam, state) => {
          let {Card.rank} = kick;
          let points = Rules.kickPoints(rank);
          switch (dealerTeam) {
          | Team.T1 => {...state, team1Points: state.team1Points + points}
          | Team.T2 => {...state, team2Points: state.team2Points + points}
          };
        };

        let dealerTeam = teamOfPlayer(state.dealer);

        // let allPlayersCards: list((Player.id, Card.t)) =
        //   [
        //     (Player.P1, state.p1Hand),
        //     (P2, state.p2Hand),
        //     (P3, state.p3Hand),
        //     (P4, state.p4Hand),
        //   ]
        //   |> List.map(((player, hand)) => handToPlayerCards(player, hand))
        //   |> List.fold_left((acc, playerHand) => acc @ playerHand, []);

        ReasonReact.Update(
          {
            ...state,
            deck,
            maybeTrumpCard: Some(kick),
            canBeg: true,
            canStand: true,
            canDeal: false,
          }
          |> updateKickPoints(kick, dealerTeam),
        );
      | EndTrick =>
        let (p1Index, p2Index, p3Index, p4Index) =
          playerBoardIndices(state.leader);
        let trick =
          Trick.{
            p1Card: List.nth(state.board, p1Index),
            p2Card: List.nth(state.board, p2Index),
            p3Card: List.nth(state.board, p3Index),
            p4Card: List.nth(state.board, p4Index),
          }; /* Action requires that the board has four cards. #unsafe*/
        Js.log(Trick.stringOfTrick(trick));
        let {Card.suit: leadSuit} = Js.Option.getExn(state.maybeLeadCard); /* Action requires leadSlot to be filled. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard); /* Action requires trumpCardSlot to be filled. #unsafe */
        let state =
          switch (Trick.playerTakesTrick(trumpSuit, leadSuit, trick)) {
          | P1 =>
            Js.log("Player 1 takes trick");
            {
              ...state,
              p1Tricks: state.p1Tricks @ [trick],
              maybePlayerTurn: Some(P1),
              leader: P1,
            };
          | P2 =>
            Js.log("Player 2 takes trick");
            {
              ...state,
              p2Tricks: state.p2Tricks @ [trick],
              maybePlayerTurn: Some(P2),
              leader: P2,
            };
          | P3 =>
            Js.log("Player 3 takes trick");
            {
              ...state,
              p3Tricks: state.p3Tricks @ [trick],
              maybePlayerTurn: Some(P3),
              leader: P3,
            };
          | P4 =>
            Js.log("Player 4 takes trick");
            {
              ...state,
              p4Tricks: state.p4Tricks @ [trick],
              maybePlayerTurn: Some(P4),
              leader: P4,
            };
          };

        /** Initialize new round if this is the last trick in the round (some player has no cards) */
        // let (state, effect) =
        //   List.length(state.p1Hand) == 0
        //     ? (
        //       {
        //         ...state,
        //         deck: Deck.make() |> Deck.shuffle,
        //         canDeal: true,
        //         dealer: Player.nextPlayer(state.dealer),
        //         leader: state.dealer |> Player.nextPlayer |> Player.nextPlayer,
        //         maybePlayerTurn: None,
        //         maybeTrumpCard: None,
        //         p1Tricks: [],
        //         p2Tricks: [],
        //         p3Tricks: [],
        //         p4Tricks: [],
        //       },
        //       (_ => ()),
        //     )
        //     : (state, (_ => ()));
        // List.length(state.p1Hand) == 0
        //   ? ReasonReact.UpdateWithSideEffects(
        //       {...state, board: [], maybeLeadCard: None},
        //       ({send}) => send(EndRound),
        //     )
        //   : ReasonReact.Update({...state, board: [], maybeLeadCard: None});
        List.length(state.p1Hand)
        == 0
          ? ReasonReact.UpdateWithSideEffects(
              {...state, board: [], maybeLeadCard: None},
              ({send}) => send(EndRound),
            )
          : ReasonReact.Update({...state, board: [], maybeLeadCard: None});
      | EndRound =>
        /* @endround start */
        Js.log("Endround triggered.");
        let team1Tricks = state.p1Tricks @ state.p3Tricks;
        let team2Tricks = state.p2Tricks @ state.p4Tricks;
        let playersCards: list((Player.id, Card.t)) =
          team1Tricks
          @ team2Tricks
          |> List.map(trickToPlayerCards)
          |> List.concat;

        Js.log("Crashes here");
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
          | None => {...state, teamJack: None}
          | Some(playerJack) =>
            let (player, _jack) = playerJack;
            let isJackHanged = isPlayerJackHangedTest(playerJack);
            switch (teamOfPlayer(player)) {
            | T1 =>
              isJackHanged
                ? {...state, teamJack: Some((T2, ThreePoints))}
                : {...state, teamJack: Some((T1, OnePoint))}
            | T2 =>
              isJackHanged
                ? {...state, teamJack: Some((T1, ThreePoints))}
                : {...state, teamJack: Some((T2, OnePoint))}
            };
          };

        let state =
          switch (lowPlayerCard) {
          | None => {...state, teamLow: None}
          | Some((player, _card)) => {
              ...state,
              teamLow: Some(teamOfPlayer(player)),
            }
          };

        let state =
          switch (highPlayerCard) {
          | None => {...state, teamHigh: None}
          | Some((player, _card)) => {
              ...state,
              teamHigh: Some(teamOfPlayer(player)),
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
        let teamGame =
          team1Points == team2Points
            ? teamOfPlayer(state.dealer)
            : team1Points > team2Points ? Team.T1 : Team.T2;
        let state = {...state, teamGame, phase: RoundSummary};
        Js.log("End EndRound");
        ReasonReact.Update(state); /* @endround end */
      | Beg =>
        Js.log("Ah beg");
        ReasonReact.Update({
          ...state,
          canBeg: false,
          canStand: false,
          canDealMore: true,
          canGiveOne: true,
        });
      | Stand =>
        Js.log("Ah stan up");
        ReasonReact.Update({
          ...state,
          canBeg: false,
          canStand: false,
          maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
        });
      | GiveOne =>
        Js.log("Take one");
        let state =
          switch (state.dealer) {
          | P1
          | P3 => {...state, team2Points: state.team2Points + 1}
          | P2
          | P4 => {...state, team1Points: state.team1Points + 1}
          };
        ReasonReact.Update({
          ...state,
          canDealMore: false,
          canGiveOne: false,
          maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
        });
      | DealMore =>
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
        let points = Rules.kickPoints(kickRank');
        let state =
          switch (teamOfPlayer(state.dealer)) {
          | Team.T1 => {...state, team1Points: state.team1Points + points}
          | Team.T2 => {...state, team2Points: state.team2Points + points}
          };
        let state =
          kickSuit == kickSuit'
            ? state
            : {
              ...state,
              canGiveOne: false,
              canDealMore: false,
              maybePlayerTurn: Some(Player.nextPlayer(state.dealer)),
            };
        ReasonReact.Update({
          ...state,
          p1Hand: state.p1Hand @ p1Hand,
          p2Hand: state.p2Hand @ p2Hand,
          p3Hand: state.p3Hand @ p3Hand,
          p4Hand: state.p4Hand @ p4Hand,
          deck: deck @ [prevKick],
          maybeTrumpCard: Some(kick'),
        });
      | BlockPlay(player) =>
        switch (player) {
        | P1 => Js.log("Not your turn p1")
        | P2 => Js.log("Not your turn p2")
        | P3 => Js.log("Not your turn p3")
        | P4 => Js.log("Not your turn p4")
        };

        ReasonReact.NoUpdate;
      },
    render: ({state, send}) => {
      let renderBegButton = playerId =>
        if (state.canBeg && Player.nextPlayer(state.dealer) == playerId) {
          <button onClick={_event => send(Beg)}>
            {ReasonReact.string("Beg")}
          </button>;
        } else {
          <div />;
        };

      let createStandElement = playerId =>
        state.canStand && playerId == Player.nextPlayer(state.dealer)
          ? <div>
              <button onClick={_event => send(Stand)}>
                {ReasonReact.string("Stand")}
              </button>
            </div>
          : <div />;

      let createDealMoreElement = playerId =>
        state.canDealMore && playerId == state.dealer
          ? <div>
              <button onClick={_event => send(DealMore)}>
                {ReasonReact.string("Deal more")}
              </button>
            </div>
          : <div />;

      let createGiveOneElement = playerId =>
        state.canGiveOne && playerId == state.dealer
          ? <div>
              <button onClick={_event => send(GiveOne)}>
                {ReasonReact.string("Give One")}
              </button>
            </div>
          : <div />;

      let createDealerElement = playerId =>
        playerId == state.dealer
          ? {
            state.canDeal
              ? <button onClick={_event => send(Deal)}>
                  {ReasonReact.string("Deal")}
                </button>
              : <span> {ReasonReact.string("Dealer")} </span>;
          }
          : ReasonReact.null;

      let createPlayerTricks = tricks =>
        <div className="column">
          {List.length(tricks) == 0
             ? <div> {ReasonReact.string("No tricks")} </div>
             : <div>
                 {List.map(
                    trick =>
                      <div
                        key={Trick.stringOfTrick(trick)} className="section">
                        <Trick trick />
                      </div>,
                    tricks,
                  )
                  |> Belt.List.toArray
                  |> ReasonReact.array}
               </div>}
        </div>;
      <div>
        <div className="section columns">
          <div className="column">
            {switch (state.phase) {
             | Game => ReasonReact.null
             | RoundSummary =>
               <div>
                 <div>
                   {ReasonReact.string(
                      Js.Option.isNone(state.teamHigh)
                        ? "No one has high"
                        : stringOfTeam(Js.Option.getExn(state.teamHigh))
                          ++ " has high.",
                    )}
                 </div>
                 <div>
                   {ReasonReact.string(
                      Js.Option.isNone(state.teamLow)
                        ? "No one has low"
                        : stringOfTeam(Js.Option.getExn(state.teamLow))
                          ++ " has low.",
                    )}
                 </div>
                 <div>
                   {switch (state.teamJack) {
                    | None => ReasonReact.null
                    | Some((team, value)) =>
                      switch (value) {
                      | ThreePoints =>
                        <div>
                          {ReasonReact.string(
                             stringOfTeam(team) ++ " hanged the jack.",
                           )}
                        </div>
                      | OnePoint =>
                        <div>
                          {ReasonReact.string(
                             stringOfTeam(team) ++ " gets away with jack.",
                           )}
                        </div>
                      }
                    }}
                 </div>
                 <div>
                   {ReasonReact.string(
                      stringOfTeam(state.teamGame) ++ " gets game.",
                    )}
                 </div>
                 <button onClick={_event => send(NewRound)}>
                   {ReasonReact.string("Continue")}
                 </button>
               </div>
             }}
          </div>
          <div className="column">
            <h1> {ReasonReact.string("Deck")} </h1>
            <ul
              // {List.map(
              //    c => <Card key={Card.stringOfCard(c)} card=c />,
              //    state.deck,
              //  )
              //  |> Belt.List.toArray
              //  |> ReasonReact.array}
            />
            <div>
              {ReasonReact.string(string_of_int(List.length(state.deck)))}
            </div>
          </div>
          <div className="column">
            <h1>
              {ReasonReact.string(
                 "Team 1 points: " ++ string_of_int(state.team1Points),
               )}
            </h1>
            <h1>
              {ReasonReact.string(
                 "Team 2 points: " ++ string_of_int(state.team2Points),
               )}
            </h1>
          </div>
          <div className="column">
            <h1> {ReasonReact.string("Trump")} </h1>
            {switch (state.maybeTrumpCard) {
             | None => <h2> {ReasonReact.string("No trump")} </h2>
             | Some(kick) => <Card card=kick />
             }}
          </div>
          <div className="column">
            <h2> {ReasonReact.string("Board")} </h2>
            {List.length(state.board) == 0
               ? <div> {ReasonReact.string("No cards on the board")} </div>
               : <div />}
            <ul>
              {List.map(
                 c =>
                   <Card
                     key={Card.stringOfCard(c)}
                     card=c
                     clickAction=?None
                   />,
                 state.board,
               )
               |> Belt.List.toArray
               |> ReasonReact.array}
            </ul>
          </div>
        </div>
        <div className="section columns">
          <div className="column">
            <h1>
              {ReasonReact.string("Player 1 ")}
              {createDealerElement(P1)}
            </h1>
            {renderBegButton(P1)}
            {createStandElement(P1)}
            {createGiveOneElement(P1)}
            {createDealMoreElement(P1)}
            <Hand
              leadCardSlot={state.maybeLeadCard}
              trumpCardSlot={state.maybeTrumpCard}
              isPlayerTurn={isPlayerTurn(state.maybePlayerTurn, P1)}
              sendPlayCard={c => send(PlayCard(P1, state.p1Hand, c))}
              cards={state.p1Hand}
            />
          </div>
          <div className="column">
            <h1>
              {ReasonReact.string("Player 2 ")}
              {createDealerElement(P2)}
            </h1>
            {renderBegButton(P2)}
            {createStandElement(P2)}
            {createGiveOneElement(P2)}
            {createDealMoreElement(P2)}
            <Hand
              leadCardSlot={state.maybeLeadCard}
              trumpCardSlot={state.maybeTrumpCard}
              isPlayerTurn={isPlayerTurn(state.maybePlayerTurn, P2)}
              sendPlayCard={c => send(PlayCard(P2, state.p2Hand, c))}
              cards={state.p2Hand}
            />
          </div>
          <div className="column">
            <h1>
              {ReasonReact.string("Player 3 ")}
              {createDealerElement(P3)}
            </h1>
            {renderBegButton(P3)}
            {createStandElement(P3)}
            {createGiveOneElement(P3)}
            {createDealMoreElement(P3)}
            <Hand
              leadCardSlot={state.maybeLeadCard}
              trumpCardSlot={state.maybeTrumpCard}
              isPlayerTurn={isPlayerTurn(state.maybePlayerTurn, P3)}
              sendPlayCard={c => send(PlayCard(P3, state.p3Hand, c))}
              cards={state.p3Hand}
            />
          </div>
          <div className="column">
            <h1>
              {ReasonReact.string("Player 4 ")}
              {createDealerElement(P4)}
            </h1>
            {renderBegButton(P4)}
            {createStandElement(P4)}
            {createGiveOneElement(P4)}
            {createDealMoreElement(P4)}
            <Hand
              leadCardSlot={state.maybeLeadCard}
              trumpCardSlot={state.maybeTrumpCard}
              isPlayerTurn={isPlayerTurn(state.maybePlayerTurn, P4)}
              sendPlayCard={c => send(PlayCard(P4, state.p4Hand, c))}
              cards={state.p4Hand}
            />
          </div>
        </div>
        <h2> {ReasonReact.string("Tricks")} </h2>
        <div className="section columns">
          <div className="column"> {createPlayerTricks(state.p1Tricks)} </div>
          <div className="column"> {createPlayerTricks(state.p2Tricks)} </div>
          <div className="column"> {createPlayerTricks(state.p3Tricks)} </div>
          <div className="column"> {createPlayerTricks(state.p4Tricks)} </div>
        </div>
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
