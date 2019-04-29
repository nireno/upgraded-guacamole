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
  
  let maybeSuitOfString = fun
  | "Clubs" => Some(Clubs)
  | "Diamonds" => Some(Diamonds)
  | "Spades" => Some(Spades)
  | "Hearts" => Some(Hearts)
  | _ => None

  let indexOfSuit = fun
  | Spades => 0
  | Hearts => 1
  | Diamonds => 2
  | Clubs => 3

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

  let indexOfRank =
    fun
    | Ace => 0
    | Two => 1
    | Three => 2
    | Four => 3
    | Five => 4
    | Six => 5
    | Seven => 6
    | Eight => 7
    | Nine => 8
    | Ten => 9
    | Jack => 10
    | Queen => 11
    | King => 12;

  let maybeRankOfInt =
    fun
    | 2 => Some(Two)
    | 3 => Some(Three)
    | 4 => Some(Four)
    | 5 => Some(Five)
    | 6 => Some(Six)
    | 7 => Some(Seven)
    | 8 => Some(Eight)
    | 9 => Some(Nine)
    | 10 => Some(Ten)
    | 11 => Some(Jack)
    | 12 => Some(Queen)
    | 13 => Some(King)
    | 14 => Some(Ace)
    | _ => None

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

[@bs.deriving {jsConverter: newType}]
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

let stringOfSpriteOffset = ( {rank, suit} ) => {
  let xSpacing = 195; // image pixels between each rank in a suit
  let ySpacing = 285; // image pixels between each suit
  let xOffset = Rank.indexOfRank(rank) * xSpacing; 
  let yOffset = Suit.indexOfSuit(suit) * ySpacing; 
  "-" ++ string_of_int(xOffset) ++ "px " ++ "-" ++ string_of_int(yOffset) ++ "px"
}

let make = (~card, ~clickAction=?, _children) => {
  ...component,
  render: _self => {
    let (clickAction, isClickable) =
      switch (clickAction) {
      | None => (ignore, false)
      | Some(ca) => (ca, true)
      };
    let style = ReactDOMRe.Style.make(~backgroundPosition=stringOfSpriteOffset(card), ());
    <div
      style
      className={"card " ++ (isClickable ? "bg-blue text-white cursor-pointer": "")}
      onClick={_event => clickAction(card)}>
    </div>;
  },
  // reducer: (action, state) =>
  //   switch (action) {
  //   | Click => ReasonReact.Update({...state, board: state.board @ [c]})
  //   },
  // initialState: () => {
  //   {card: (Four, Hearts)};
  // },
};
