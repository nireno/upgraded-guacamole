// let component = ReasonReact.reducerComponent("Card");
let component = ReasonReact.statelessComponent("Card");

module Suit = {
  type t =
    | Clubs
    | Diamonds
    | Spades
    | Hearts;

  let toString =
    fun
    | Clubs => "Clubs"
    | Diamonds => "Diamonds"
    | Spades => "Spades"
    | Hearts => "Hearts";

  let first = Clubs;
  let last = Hearts;
  let next =
    fun
    | Clubs => Diamonds
    | Diamonds => Spades
    | Spades => Hearts
    | Hearts => Clubs;

  let listOfSuits = () => {
    let rec loop = (v, l) => {
      switch (v) {
      | x when x == last => l @ [x]
      | x => loop(next(x), l @ [x])
      };
    };
    loop(first, []);
  };
};

module Rank = {
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace;

  let stringOfRank =
    fun
    | Ace => "Ace"
    | Two => "Two"
    | Three => "Three"
    | Four => "Four"
    | Five => "Five"
    | Six => "Six"
    | Seven => "Seven"
    | Eight => "Eight"
    | Nine => "Nine"
    | Ten => "Ten"
    | Jack => "Jack"
    | Queen => "Queen"
    | King => "King";

  let first = Ace;
  let last = King;
  let next =
    fun
    | Ace => Two
    | Two => Three
    | Three => Four
    | Four => Five
    | Five => Six
    | Six => Seven
    | Seven => Eight
    | Eight => Nine
    | Nine => Ten
    | Ten => Jack
    | Jack => Queen
    | Queen => King
    | King => Ace;

  let listOfRanks = () => {
    let rec loop = (v, l) => {
      switch (v) {
      | x when x == last => l @ [x]
      | x => loop(next(x), l @ [x])
      };
    };
    loop(first, []);
  };
};

let stringOfCard = ((rank, suit)) =>
  Rank.stringOfRank(rank) ++ " of " ++ Suit.toString(suit);

type t = (Rank.t, Suit.t);

type state = {card: t};

// type action =
//   | Click;

let make = (~card, ~onCardSelected=?, _children) => {
  ...component,
  render: _self => {
    let onClick =
      switch (onCardSelected) {
      | Some(onCardSelected) => Some(_event => onCardSelected(card))
      | None => None
      };
    <li ?onClick> {ReasonReact.string(stringOfCard(card))} </li>;
  },
  // reducer: (action, state) =>
  //   switch (action) {
  //   | Click => ReasonReact.Update({...state, board: state.board @ [c]})
  //   },
  // initialState: () => {
  //   {card: (Four, Hearts)};
  // },
};
