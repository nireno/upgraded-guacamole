/** One trick is completed when each player play's one card on the board */;

type partialTrick = {
  p1CardSlot: option(Card.t),
  p2CardSlot: option(Card.t),
  p3CardSlot: option(Card.t),
  p4CardSlot: option(Card.t),
};

let listOfTrick: partialTrick => list((Player.id, option(Card.t))) =
  r => [
    (Player.P1, r.p1CardSlot),
    (Player.P2, r.p2CardSlot),
    (Player.P3, r.p3CardSlot),
    (Player.P4, r.p4CardSlot),
  ];

let stringOfTrick = r => {
  let stringOfPlayerCardSlot = ((playerId, cardSlot)) =>
    Player.toString(playerId) ++ ": " ++ Card.stringOfCardSlot(cardSlot);

  listOfTrick(r)
  |> List.map(stringOfPlayerCardSlot)
  |> List.fold_left((acc, s) => acc ++ " " ++ s, "");
};

let isComplete = partialTrick =>
  Belt.List.every(
    listOfTrick(partialTrick)
    |> List.map(((_playerId, cardSlot: option(Card.t))) => cardSlot),
    Belt.Option.isSome,
  );
