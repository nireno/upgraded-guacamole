/** One trick is completed when each player play's one card on the board */;

[@decco]
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
    (N1, r.p1Card),
    (N2, r.p2Card),
    (N3, r.p3Card),
    (N4, r.p4Card),
  ];

let stringOfTrick = r => {
  let stringOfPlayerCard = ((playerId, card)) =>
    Player.stringOfId(playerId) ++ ": " ++ Card.stringOfCard(card);

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

[@react.component]
let make = (~trick) => {
  <ul>
    <li> {ReasonReact.string("P1: " ++ Card.stringOfCard(trick.p1Card))} </li>
    <li> {ReasonReact.string("P2: " ++ Card.stringOfCard(trick.p2Card))} </li>
    <li> {ReasonReact.string("P3: " ++ Card.stringOfCard(trick.p3Card))} </li>
    <li> {ReasonReact.string("P4: " ++ Card.stringOfCard(trick.p4Card))} </li>
  </ul>;
};

let getValue = trick =>
  cardsInTrick(trick)
  |> List.fold_left((acc, {Card.rank}) => acc + Card.Rank.pointsOfRank(rank), 0);
