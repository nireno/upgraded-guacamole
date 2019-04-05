/** One trick is completed when each player play's one card on the board */;

type t = {
  p1Card: Card.t,
  p2Card: Card.t,
  p3Card: Card.t,
  p4Card: Card.t,
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

// let trickHasSuitTest = (suit, trick) => {
//   let (_rank, p1Suit) = trick.p1Card;
//   let (_rank, p2Suit) = trick.p2Card;
//   let (_rank, p3Suit) = trick.p3Card;
//   let (_rank, p4Suit) = trick.p4Card;

//   p1Suit == suit || p2Suit == suit || p3Suit == suit || p4Suit == suit;
// };

let winner = (testSuit, trick) => {
  let accHighestRankedPlayer = (acc, (player', (rank', suit'))) => {
    switch (acc) {
    | None => Some((player', (rank', suit')))
    | Some((player, (rank, suit))) =>
      if (Card.Rank.(intOfRank(rank') > intOfRank(rank))) {
        Some((player', (rank', suit')));
      } else {
        Some((player, (rank, suit)));
      }
    };
  };

  listOfTrick(trick)
  |> List.filter(((_player, (_rank, suit))) => suit == testSuit)
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
