let suits = Card.Suit.listOfSuits();
let ranks = Card.Rank.listOfRanks();

type t = list(Card.t);

let make = () =>
  List.map(suit => List.map(rank => Card.{rank, suit}, ranks), suits)
  |> List.flatten;

// https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml

let shuffle = My.List.shuffle;

let deal = (n, d) => (
  Belt.List.take(d, n) |> Js.Option.getWithDefault([]),
  Belt.List.drop(d, n) |> Js.Option.getWithDefault(d),
);

let deal1Exn = (d) => (List.hd(d), List.tl(d));

let testFinalTrumpTransitionToRedeal = {
  Card.[
    {suit: Suit.Clubs, rank: Rank.Two},
      {suit: Suit.Clubs, rank: Rank.Three},
      {suit: Suit.Clubs, rank: Rank.Four},
      {suit: Suit.Clubs, rank: Rank.Five},
      {suit: Suit.Clubs, rank: Rank.Six},
      {suit: Suit.Clubs, rank: Rank.Seven},
      {suit: Suit.Clubs, rank: Rank.Eight},
      {suit: Suit.Clubs, rank: Rank.Nine},
      {suit: Suit.Clubs, rank: Rank.Ten},
      {suit: Suit.Clubs, rank: Rank.Jack},
      {suit: Suit.Clubs, rank: Rank.Queen},
      {suit: Suit.Clubs, rank: Rank.King},
      {suit: Suit.Clubs, rank: Rank.Ace},
  ];
};

let testFinalTrumpTransitionToPlayerTurn = {
  Card.[
    {suit: Suit.Clubs, rank: Rank.Two},
      {suit: Suit.Clubs, rank: Rank.Three},
      {suit: Suit.Clubs, rank: Rank.Four},
      {suit: Suit.Clubs, rank: Rank.Five},
      {suit: Suit.Clubs, rank: Rank.Six},
      {suit: Suit.Clubs, rank: Rank.Seven},
      {suit: Suit.Clubs, rank: Rank.Eight},
      {suit: Suit.Clubs, rank: Rank.Nine},
      {suit: Suit.Clubs, rank: Rank.Ten},
      {suit: Suit.Clubs, rank: Rank.Jack},
      {suit: Suit.Spades, rank: Rank.Queen},
  ];
};

let testFinalTrumpTransitionToGameOver = {
  Card.[
    {suit: Suit.Clubs, rank: Rank.Two},
      {suit: Suit.Clubs, rank: Rank.Three},
      {suit: Suit.Clubs, rank: Rank.Four},
      {suit: Suit.Clubs, rank: Rank.Five},
      {suit: Suit.Clubs, rank: Rank.Six},
      {suit: Suit.Clubs, rank: Rank.Seven},
      {suit: Suit.Clubs, rank: Rank.Eight},
      {suit: Suit.Clubs, rank: Rank.Nine},
      {suit: Suit.Clubs, rank: Rank.Ten},
      {suit: Suit.Clubs, rank: Rank.Jack},
      {suit: Suit.Spades, rank: Rank.Jack},
  ];
};