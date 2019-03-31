let suits = Card.Suit.listOfSuits();
let ranks = Card.Rank.listOfRanks();

type t = list(Card.t);

let make = () =>
  List.map(suit => List.map(rank => (rank, suit), ranks), suits)
  |> List.flatten;

// https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml

let shuffle = d => {
  Random.self_init();
  let nd = List.map(c => (Random.bits(), c), d);
  let sond = List.sort(compare, nd);
  List.map(snd, sond);
};

let deal = (n, d) => (
  Belt.List.take(d, n) |> Js.Option.getWithDefault([]),
  Belt.List.drop(d, n) |> Js.Option.getWithDefault(d),
);
