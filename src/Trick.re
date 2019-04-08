/** One trick is completed when each player play's one card on the board */;

type t = {
  p1Card: Card.t,
  p2Card: Card.t,
  p3Card: Card.t,
  p4Card: Card.t,
};

let cardsInTrick = trick => {
  [trick.p1Card, trick.p2Card, trick.p3Card, trick.p4Card]
};

let listOfTrick: t => list((Player.id, Card.t)) =
  r => [
    (Player.P1, r.p1Card),
    (Player.P2, r.p2Card),
    (Player.P3, r.p3Card),
    (Player.P4, r.p4Card),
  ];

let stringOfTrick = r => {
  let stringOfPlayerCard = ((playerId, card)) =>
    Player.toString(playerId) ++ ": " ++ Card.stringOfCard(card);

  listOfTrick(r)
  |> List.map(stringOfPlayerCard)
  |> List.fold_left((acc, s) => acc ++ " " ++ s, "");
};

let winner: (Card.Suit.t, t) => option((Player.id, Card.t)) =
  (testSuit, trick) => {
    let accHighestRankedPlayer =
        (acc, (player', {Card.rank: rank', Card.suit: suit'})) => {
      switch (acc) {
      | None => Some((player', {Card.rank: rank', suit: suit'}))
      | Some((player, {Card.rank, suit})) =>
        if (Card.Rank.(intOfRank(rank') > intOfRank(rank))) {
          Some((player', {Card.rank: rank', suit: suit'}));
        } else {
          Some((player, {rank, suit}));
        }
      };
    };

    listOfTrick(trick)
    |> List.filter(((_player, {Card.suit})) => suit == testSuit)
    |> List.fold_left(accHighestRankedPlayer, None);
  };

let playerTakesTrick = (trumpSuit, leaderSuit, trick) => {
  switch (winner(trumpSuit, trick)) {
  | None =>
    switch (winner(leaderSuit, trick)) {
    | None =>
      failwith(
        "player-takes-trick: failed to determine the winner of the trick",
      )
    | Some((player, _)) => player
    }
  | Some((player, _)) => player
  };
};

let component = ReasonReact.statelessComponent("Trick");

let make = (~trick, _children) => {
  {
    ...component,
    render: _self => {
      <ul>
        <li>
          {ReasonReact.string("P1: " ++ Card.stringOfCard(trick.p1Card))}
        </li>
        <li>
          {ReasonReact.string("P2: " ++ Card.stringOfCard(trick.p2Card))}
        </li>
        <li>
          {ReasonReact.string("P3: " ++ Card.stringOfCard(trick.p3Card))}
        </li>
        <li>
          {ReasonReact.string("P4: " ++ Card.stringOfCard(trick.p4Card))}
        </li>
      </ul>;
    },
  };
};
