[@bs.val] external node_env: string = "process.env.NODE_ENV";
open Game;

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
    | Deal
    | EndGame
    | CheatPoints(Team.id, int);

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
        maybeTeamHigh: None,
        maybeTeamLow: None,
        maybeTeamJack: None,
        maybeTeamGame: None,
        phase: Game,
      };
    },
    reducer: (action, state) =>
      switch (action) {
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
          Util.updateUntil(
            [
              maybeAddPoints(state.maybeTeamHigh, valueOfAward(High)),
              maybeAddPoints(state.maybeTeamLow, valueOfAward(Low)),
              maybeAddJackPoints(state.maybeTeamJack),
              maybeAddPoints(state.maybeTeamGame, valueOfAward(Game)),
              resetBoard,
            ],
            isGameOverTest,
            state,
          );

        let state =
          isGameOverTest(state) ? {...state, phase: GameOver} : state;

        switch (state.phase) {
        | GameOver =>
          ReasonReact.UpdateWithSideEffects(
            state,
            ({send}) => send(EndGame),
          )
        | _ => ReasonReact.Update(state)
        };

      | EndGame => ReasonReact.Update({...state, canDeal: false})
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
        let dealCards = state => {
          let (p1Hand, deck) = Deck.deal(2, state.deck);
          let (p2Hand, deck) = Deck.deal(2, deck);
          let (p3Hand, deck) = Deck.deal(2, deck);
          let (p4Hand, deck) = Deck.deal(2, deck);
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

        isGameOverTest(state)
          ? ReasonReact.UpdateWithSideEffects(
              {...state, phase: GameOver},
              ({send}) => send(EndGame),
            )
          : ReasonReact.Update({
              ...state,
              canBeg: true,
              canStand: true,
              canDeal: false,
            });
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
        let {Card.suit: leadSuit} = Js.Option.getExn(state.maybeLeadCard); /* Action requires leadCard. #unsafe */
        let {Card.suit: trumpSuit} = Js.Option.getExn(state.maybeTrumpCard); /* Action requires trumpCard. #unsafe */

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
          maybePlayerTurn: Some(trickWinner),
          maybeLeadCard: None,
          leader: trickWinner,
          board: [],
        };

        let state = state |> collectTrick(trickWinner, trick) |> advanceRound;

        List.length(state.p1Hand) == 0
          ? ReasonReact.UpdateWithSideEffects(
              state,
              ({send}) => send(EndRound),
            )
          : ReasonReact.Update(state);

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
          | None => {...state, maybeTeamJack: None}
          | Some(playerJack) =>
            let (player, _jack) = playerJack;
            let isJackHanged = isPlayerJackHangedTest(playerJack);
            switch (teamOfPlayer(player)) {
            | T1 =>
              isJackHanged
                ? {...state, maybeTeamJack: Some((T2, HangJack))}
                : {...state, maybeTeamJack: Some((T1, RunJack))}
            | T2 =>
              isJackHanged
                ? {...state, maybeTeamJack: Some((T1, HangJack))}
                : {...state, maybeTeamJack: Some((T2, RunJack))}
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
        let state = {...state, maybeTeamGame, phase: RoundSummary};
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
        let points = kickPoints(kickRank');
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
      | CheatPoints(team, value) =>
        ReasonReact.Update(addPoints(team, value, state))
      },
    render: self => {
      let {ReasonReact.state, send} = self;
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

      let createCheatPoints = team => {
        node_env != "production"
          ? <span>
              <button onClick={_event => send(CheatPoints(team, 1))}>
                {ReasonReact.string("+1")}
              </button>
              <button onClick={_event => send(CheatPoints(team, 6))}>
                {ReasonReact.string("+6")}
              </button>
              <button onClick={_event => send(CheatPoints(team, 13))}>
                {ReasonReact.string("+13")}
              </button>
            </span>
          : ReasonReact.null;
      };

      <div>
        <div className="section columns">
          <div className="column">
            {switch (state.phase) {
             | Game => ReasonReact.null
             | GameOver => GameOverPhase.createElement(self)
             | RoundSummary =>
               <div>
                 <div>
                   {ReasonReact.string(
                      switch (state.maybeTeamHigh) {
                      | None => "No one has high"
                      | Some(teamHigh) =>
                        Team.stringOfTeam(teamHigh) ++ " has high."
                      },
                    )}
                 </div>
                 <div>
                   {ReasonReact.string(
                      switch (state.maybeTeamLow) {
                      | None => "No one has low"
                      | Some(teamLow) =>
                        Team.stringOfTeam(teamLow) ++ " has low."
                      },
                    )}
                 </div>
                 <div>
                   {switch (state.maybeTeamJack) {
                    | None => ReasonReact.null
                    | Some((team, value)) =>
                      switch (value) {
                      | HangJack =>
                        <div>
                          {ReasonReact.string(
                             Team.stringOfTeam(team) ++ " hanged the jack.",
                           )}
                        </div>
                      | RunJack =>
                        <div>
                          {ReasonReact.string(
                             Team.stringOfTeam(team)
                             ++ " gets away with jack.",
                           )}
                        </div>
                      | _ => ReasonReact.null
                      }
                    }}
                 </div>
                 <div>
                   {switch (state.maybeTeamGame) {
                    | None => ReasonReact.string("Tied for game.")
                    | Some(teamGame) =>
                      ReasonReact.string(
                        Team.stringOfTeam(teamGame) ++ " gets game.",
                      )
                    }}
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
                 "Team 1 points: " ++ string_of_int(state.team1Points) ++ " ",
               )}
              {createCheatPoints(T1)}
            </h1>
            <h1>
              {ReasonReact.string(
                 "Team 2 points: " ++ string_of_int(state.team2Points) ++ " ",
               )}
              {createCheatPoints(T2)}
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
