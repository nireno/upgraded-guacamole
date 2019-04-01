type turn = option(Player.id);

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

module App = {
  type action =
    | PlayCard(Player.id, Player.hand, Card.t)
    | BlockPlay(Player.id)
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
    kick: option(Card.t),
    me: Player.id,
    dealer: Player.id,
    turn,
    team1Points: int,
    team2Points: int,
    canBeg: bool,
    canStand: bool,
    canDealMore: bool,
    canGiveOne: bool,
  };

  let component = ReasonReact.reducerComponent("AllFoursApp");

  let make = _children => {
    ...component,
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
        Js.log("Playing card " ++ Card.stringOfCard(c));
        let state = {...state, board: state.board @ [c]};
        let state =
          switch (state.turn) {
          | None => state
          | Some(turn) => {...state, turn: Some(Player.nextPlayer(turn))}
          };

        player == state.dealer
          ? ReasonReact.UpdateWithSideEffects(
              state,
              ({send}) => send(EndRound),
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
          {...state, deck, kick: Some(kick), canBeg: true, canStand: true}
          |> updateKickPoints(kick, dealerTeam),
        );
      | EndRound => ReasonReact.Update({...state, board: []})
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
          turn: Some(Player.nextPlayer(state.dealer)),
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
          turn: Some(Player.nextPlayer(state.dealer)),
        });
      | DealMore =>
        Js.log("I beg too");
        let (p1Hand, deck) = Deck.deal(3, state.deck);
        let (p2Hand, deck) = Deck.deal(3, deck);
        let (p3Hand, deck) = Deck.deal(3, deck);
        let (p4Hand, deck) = Deck.deal(3, deck);
        let prevKick =
          switch (state.kick) {
          | None =>
            failwith(
              "DealMore action expected state.kick to be Some thing but got None",
            )
          | Some(k) => k
          };

        let kick = Js.Option.getExn(state.kick);
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
              turn: Some(Player.nextPlayer(state.dealer)),
            };
        ReasonReact.Update({
          ...state,
          p1Hand: state.p1Hand @ p1Hand,
          p2Hand: state.p2Hand @ p2Hand,
          p3Hand: state.p3Hand @ p3Hand,
          p4Hand: state.p4Hand @ p4Hand,
          deck: deck @ [prevKick],
          kick: Some(kick'),
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
    initialState: () => {
      {
        me: P1,
        deck: Deck.make() |> Deck.shuffle,
        board: [],
        p1Hand: [],
        p2Hand: [],
        p3Hand: [],
        p4Hand: [],
        turn: None,
        kick: None,
        dealer: P1,
        team1Points: 0,
        team2Points: 0,
        canBeg: false,
        canStand: false,
        canDealMore: false,
        canGiveOne: false,
      };
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
          ? <div> {ReasonReact.string("Dealer")} </div> : <div />;

      <div>
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
        <button onClick={_event => send(Deal)}>
          {ReasonReact.string("Deal")}
        </button>
        <h1> {ReasonReact.string("Game")} </h1>
        {switch (state.kick) {
         | None => <div />
         | Some(kick) =>
           <div>
             <h2> {ReasonReact.string("Kick")} </h2>
             <Card card=kick />
           </div>
         }}
        <h2> {ReasonReact.string("Board")} </h2>
        {List.length(state.board) == 0
           ? <div> {ReasonReact.string("No cards on the board")} </div>
           : <div />}
        <ul>
          {List.map(
             c => <Card key={Card.stringOfCard(c)} card=c />,
             state.board,
           )
           |> Belt.List.toArray
           |> ReasonReact.array}
        </ul>
        <h1> {ReasonReact.string("Player 1")} </h1>
        {createDealerElement(P1)}
        {renderBegButton(P1)}
        {createStandElement(P1)}
        {createGiveOneElement(P1)}
        {createDealMoreElement(P1)}
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P1)
                ? PlayCard(P1, state.p1Hand, c) : BlockPlay(P1),
            )
          }
          cards={state.p1Hand}
        />
        <h1> {ReasonReact.string("Player 2")} </h1>
        {createDealerElement(P2)}
        {renderBegButton(P2)}
        {createStandElement(P2)}
        {createGiveOneElement(P2)}
        {createDealMoreElement(P2)}
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P2)
                ? PlayCard(P2, state.p2Hand, c) : BlockPlay(P2),
            )
          }
          cards={state.p2Hand}
        />
        <h1> {ReasonReact.string("Player 3")} </h1>
        {createDealerElement(P3)}
        {renderBegButton(P3)}
        {createStandElement(P3)}
        {createGiveOneElement(P3)}
        {createDealMoreElement(P3)}
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P3)
                ? PlayCard(P3, state.p3Hand, c) : BlockPlay(P3),
            )
          }
          cards={state.p3Hand}
        />
        <h1> {ReasonReact.string("Player 4")} </h1>
        {createDealerElement(P4)}
        {renderBegButton(P4)}
        {createStandElement(P4)}
        {createGiveOneElement(P4)}
        {createDealMoreElement(P4)}
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P4)
                ? PlayCard(P4, state.p4Hand, c) : BlockPlay(P4),
            )
          }
          cards={state.p4Hand}
        />
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
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
