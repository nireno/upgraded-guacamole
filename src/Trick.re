/** One trick is completed when each player play's one card on the board */;

[@decco]
type t = (Card.t, Card.t, Card.t, Card.t);

let stringOfTrick = r => {
  let stringOfPlayerCard = ((playerId, card)) =>
    Player.stringOfId(playerId) ++ ": " ++ Card.stringOfCard(card);

  Quad.toDict(r)
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

    Quad.toDict(trick)
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
  let (c1, c2, c3, c4) = trick;
  <ul>
    <li> {ReasonReact.string("P1: " ++ Card.stringOfCard(c1))} </li>
    <li> {ReasonReact.string("P2: " ++ Card.stringOfCard(c2))} </li>
    <li> {ReasonReact.string("P3: " ++ Card.stringOfCard(c3))} </li>
    <li> {ReasonReact.string("P4: " ++ Card.stringOfCard(c4))} </li>
  </ul>;
};

let getValue = trick =>
  Quad.toList(trick)
  |> List.fold_left((acc, {Card.rank}) => acc + Card.Rank.pointsOfRank(rank), 0);
