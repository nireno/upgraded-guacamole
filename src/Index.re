type maybePlayerTurn = option(Player.id);

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

let suitOfKickUnsafe =
  fun
  | None => failwith("suitOfKickUnsafe expected Some card but got None")
  | Some(card) => snd(card);

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

module App = {
  type action =
    | PlayCard(Player.id, Player.hand, Card.t)
    | BlockPlay(Player.id)
    | EndTrick
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
    p1Take: list(Trick.t),
    p2Take: list(Trick.t),
    p3Take: list(Trick.t),
    p4Take: list(Trick.t),
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
        p1Take: [],
        p2Take: [],
        p3Take: [],
        p4Take: [],
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
      };
    },
    reducer: (action, state) =>
      switch (action) {
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
        let (p1Hand, deck) = Deck.deal(6, state.deck);
        let (p2Hand, deck) = Deck.deal(6, deck);
        let (p3Hand, deck) = Deck.deal(6, deck);
        let (p4Hand, deck) = Deck.deal(6, deck);
        let state = {...state, p1Hand, p2Hand, p3Hand, p4Hand};

        let (cards, deck) = Deck.deal(1, deck);
        let kick = List.hd(cards);
        let updateKickPoints = (kick, dealerTeam, state) => {
          let (rank, _) = kick;
          let points = Rules.kickPoints(rank);
          switch (dealerTeam) {
          | Team.T1 => {...state, team1Points: state.team1Points + points}
          | Team.T2 => {...state, team2Points: state.team2Points + points}
          };
        };

        let dealerTeam = teamOfPlayer(state.dealer);
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
          }; /* UNSAFE : action requires that the board has four cards*/
        Js.log(Trick.stringOfTrick(trick));
        let (_, leadSuit) = Js.Option.getExn(state.maybeLeadCard); /* UNSAFE : action requires leadSlot to be filled */
        let (_, trumpSuit) = Js.Option.getExn(state.maybeTrumpCard); /* UNSAFE : action requires trumpCardSlot to be filled */
        let state =
          switch (Trick.playerTakesTrick(trumpSuit, leadSuit, trick)) {
          | P1 =>
            Js.log("Player 1 takes trick");
            {
              ...state,
              p1Take: state.p1Take @ [trick],
              maybePlayerTurn: Some(P1),
              leader: P1,
            };
          | P2 =>
            Js.log("Player 2 takes trick");
            {
              ...state,
              p2Take: state.p2Take @ [trick],
              maybePlayerTurn: Some(P2),
              leader: P2,
            };
          | P3 =>
            Js.log("Player 3 takes trick");
            {
              ...state,
              p3Take: state.p3Take @ [trick],
              maybePlayerTurn: Some(P3),
              leader: P3,
            };
          | P4 =>
            Js.log("Player 4 takes trick");
            {
              ...state,
              p4Take: state.p4Take @ [trick],
              maybePlayerTurn: Some(P4),
              leader: P4,
            };
          };

        /** Initialize new round if this is the last trick in the round (some player has no cards) */
        let state =
          List.length(state.p1Hand) == 0
            ? {
              ...state,
              deck: Deck.make() |> Deck.shuffle,
              canDeal: true,
              dealer: Player.nextPlayer(state.dealer),
              leader: state.dealer |> Player.nextPlayer |> Player.nextPlayer,
              maybePlayerTurn: None,
              maybeTrumpCard: None,
              p1Take: [],
              p2Take: [],
              p3Take: [],
              p4Take: [],
            }
            : state;

        ReasonReact.Update({...state, board: [], maybeLeadCard: None});
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
        let (_, kickSuit) = kick;

        let (cards, deck) = Deck.deal(1, deck);
        let kick' = List.hd(cards);
        let (kickRank', kickSuit') = kick';
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
          ? <div>
              <div> {ReasonReact.string("Dealer")} </div>
              {state.canDeal
                 ? <button onClick={_event => send(Deal)}>
                     {ReasonReact.string("Deal")}
                   </button>
                 : ReasonReact.null}
            </div>
          : <div />;

      let createPlayerTakes = takes =>
        <div className="column">
          {List.length(takes) == 0
             ? <div> {ReasonReact.string("No take")} </div>
             : <div>
                 {List.map(
                    trick =>
                      <div
                        key={Trick.stringOfTrick(trick)} className="section">
                        <Trick trick />
                      </div>,
                    takes,
                  )
                  |> Belt.List.toArray
                  |> ReasonReact.array}
               </div>}
        </div>;

      <div>
        <div className="section columns">
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
            <h1> {ReasonReact.string("Player 1")} </h1>
            {createDealerElement(P1)}
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
            <h1> {ReasonReact.string("Player 2")} </h1>
            {createDealerElement(P2)}
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
            <h1> {ReasonReact.string("Player 3")} </h1>
            {createDealerElement(P3)}
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
            <h1> {ReasonReact.string("Player 4")} </h1>
            {createDealerElement(P4)}
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
        <h2> {ReasonReact.string("Take")} </h2>
        <div className="section columns">
          <div className="column"> {createPlayerTakes(state.p1Take)} </div>
          <div className="column"> {createPlayerTakes(state.p2Take)} </div>
          <div className="column"> {createPlayerTakes(state.p3Take)} </div>
          <div className="column"> {createPlayerTakes(state.p4Take)} </div>
        </div>
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
