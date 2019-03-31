type hand = list(Card.t);
type lift = list((Card.t, Card.t, Card.t, Card.t));

type playerNumber =
  | P1
  | P2
  | P3
  | P4;

let firstPlayer = P1;
let lastPlayer = P4;

let succPlayer =
  fun
  | P1 => P2
  | P2 => P3
  | P3 => P4
  | P4 => P1;

type player = {
  number: playerNumber,
  hand,
  lift,
};

type turn = option(playerNumber);

let isPlayerTurn = (turn, playerNumber) => {
  switch (turn) {
  | None => false
  | Some(turn) => turn == playerNumber
  };
};

// type player = {
//   hand: list(Card.t),
//   lift: list((Card.t, Card.t, Card.t, Card.t)),
// };

module App = {
  type action =
    | PlayCard(player, Card.t)
    | BlockPlay(player)
    | Deal;

  type state = {
    me: playerNumber,
    deck: Deck.t,
    board: list(Card.t),
    p1: player,
    p2: player,
    p3: player,
    p4: player,
    turn,
    kick: option(Card.t),
    // hand1: hand,
    // lift1: lift,
    // hand2: hand,
    // lift2: lift,
    // hand3: hand,
    // lift3: lift,
    // hand4: hand,
    // lift4: lift,
  };

  // let onChange = (send, event) => {
  //   let checked = ReactEvent.Form.target(event)##checked;
  //   send(PlayCard(checked));
  // };

  let component = ReasonReact.reducerComponent("AllFoursApp");

  let make = _children => {
    ...component,
    reducer: (action, state) =>
      switch (action) {
      | PlayCard(player, c) =>
        let player = {
          ...player,
          hand: List.filter(c' => c != c', player.hand),
        };
        Js.log("Playing card " ++ Card.stringOfCard(c));
        let state = {...state, board: state.board @ [c]};
        let state =
          switch (player.number) {
          | P1 => {...state, p1: player}
          | P2 => {...state, p2: player}
          | P3 => {...state, p3: player}
          | P4 => {...state, p4: player}
          };
        ReasonReact.Update(state);
      | Deal =>
        let (hand1, deck) = Deck.deal(6, state.deck);
        let (hand2, deck) = Deck.deal(6, deck);
        let (hand3, deck) = Deck.deal(6, deck);
        let (hand4, deck) = Deck.deal(6, deck);
        let (kick, deck) = Deck.deal(1, deck);

        ReasonReact.Update({
          ...state,
          deck,
          p1: {
            ...state.p1,
            hand: hand1,
          },
          p2: {
            ...state.p2,
            hand: hand2,
          },
          p3: {
            ...state.p3,
            hand: hand3,
          },
          p4: {
            ...state.p4,
            hand: hand4,
          },
          kick: Some(List.hd(kick)),
        });
      | BlockPlay(player) =>
        switch (player.number) {
        | P1 => Js.log("Not your turn p1")
        | P2 => Js.log("Not your turn p2")
        | P3 => Js.log("Not your turn p3")
        | P4 => Js.log("Not your turn p4")
        };

        ReasonReact.Update(state);
      },
    initialState: () => {
      {
        me: P1,
        deck: Deck.make() |> Deck.shuffle,
        board: [],
        p1: {
          number: P1,
          hand: [],
          lift: [],
        },
        p2: {
          number: P2,
          hand: [],
          lift: [],
        },
        p3: {
          number: P3,
          hand: [],
          lift: [],
        },
        p4: {
          number: P4,
          hand: [],
          lift: [],
        },
        turn: Some(P1),
        kick: None,
      };
    },
    render: ({state, send}) => {
      <div>
        <h1> {ReasonReact.string("Deck")} </h1>
        <ul>
          {List.map(
             c => <Card key={Card.stringOfCard(c)} card=c />,
             state.deck,
           )
           |> Belt.List.toArray
           |> ReasonReact.array}
        </ul>
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
        {List.length(state.board) > 0
           ? <h2> {ReasonReact.string("Board")} </h2> : <div />}
        <ul>
          {List.map(
             c => <Card key={Card.stringOfCard(c)} card=c />,
             state.board,
           )
           |> Belt.List.toArray
           |> ReasonReact.array}
        </ul>
        <h1> {ReasonReact.string("Player 1")} </h1>
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P1)
                ? PlayCard(state.p1, c) : BlockPlay(state.p1),
            )
          }
          cards={state.p1.hand}
        />
        <h1> {ReasonReact.string("Player 2")} </h1>
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P2)
                ? PlayCard(state.p2, c) : BlockPlay(state.p2),
            )
          }
          cards={state.p2.hand}
        />
        <h1> {ReasonReact.string("Player 3")} </h1>
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P3)
                ? PlayCard(state.p3, c) : BlockPlay(state.p3),
            )
          }
          cards={state.p3.hand}
        />
        <h1> {ReasonReact.string("Player 4")} </h1>
        <Hand
          onCardSelected={c =>
            send(
              isPlayerTurn(state.turn, P4)
                ? PlayCard(state.p4, c) : BlockPlay(state.p4),
            )
          }
          cards={state.p4.hand}
        />
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
