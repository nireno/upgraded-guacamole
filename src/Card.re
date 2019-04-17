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

  let intOfRank =
    fun
    | Two => 2
    | Three => 3
    | Four => 4
    | Five => 5
    | Six => 6
    | Seven => 7
    | Eight => 8
    | Nine => 9
    | Ten => 10
    | Jack => 11
    | Queen => 12
    | King => 13
    | Ace => 14;

  let pointsOfRank =
    fun
    | Ten => 10
    | Jack => 1
    | Queen => 2
    | King => 3
    | Ace => 4
    | _ => 0;

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

type t = {
  rank: Rank.t,
  suit: Suit.t,
};

let stringOfCard: t => string =
  ({rank, suit}) =>
    Rank.stringOfRank(rank) ++ " of " ++ Suit.toString(suit);

let stringOfMaybeCard = maybeCard =>
  switch (maybeCard) {
  | None => "Empty"
  | Some(card) => stringOfCard(card)
  };

type state = {card: t};

// type action =
//   | Click;

let make = (~card, ~clickAction=?, _children) => {
  ...component,
  render: _self => {
    let (clickAction, isClickable) =
      switch (clickAction) {
      | None => (ignore, false)
      | Some(ca) => (ca, true)
      };
    <li
      style={
        isClickable
          ? ReactDOMRe.Style.make(~border="1px solid #ff9966", ())
          : ReactDOMRe.Style.make()
      }
      onClick={_event => clickAction(card)}>
      {ReasonReact.string(stringOfCard(card))}
    </li>;
  },
  // reducer: (action, state) =>
  //   switch (action) {
  //   | Click => ReasonReact.Update({...state, board: state.board @ [c]})
  //   },
  // initialState: () => {
  //   {card: (Four, Hearts)};
  // },
};
