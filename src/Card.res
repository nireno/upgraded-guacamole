module Suit = {
  @spice
  type t =
    | Clubs
    | Diamonds
    | Spades
    | Hearts

  let codeOfSuit = x =>
    switch x {
    | Clubs => "C"
    | Diamonds => "D"
    | Spades => "S"
    | Hearts => "H"
    }

  let toString = x =>
    switch x {
    | Clubs => "Clubs"
    | Diamonds => "Diamonds"
    | Spades => "Spades"
    | Hearts => "Hearts"
    }

  let maybeSuitOfString = x =>
    switch x {
    | "Clubs" => Some(Clubs)
    | "Diamonds" => Some(Diamonds)
    | "Spades" => Some(Spades)
    | "Hearts" => Some(Hearts)
    | _ => None
    }

  let indexOfSuit = x =>
    switch x {
    | Spades => 0
    | Hearts => 1
    | Diamonds => 2
    | Clubs => 3
    }

  let first = Clubs
  let last = Hearts
  let next = x =>
    switch x {
    | Clubs => Diamonds
    | Diamonds => Spades
    | Spades => Hearts
    | Hearts => Clubs
    }

  let listOfSuits = () => {
    let rec loop = (v, l) =>
      switch v {
      | x if x == last => Belt.List.concat(l, list{x})
      | x => loop(next(x), Belt.List.concat(l, list{x}))
      }
    loop(first, list{})
  }
}

module Rank = {
  @spice
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
    | Ace

  let codeOfRank = x =>
    switch x {
    | Ace => "A"
    | Two => "2"
    | Three => "3"
    | Four => "4"
    | Five => "5"
    | Six => "6"
    | Seven => "7"
    | Eight => "8"
    | Nine => "9"
    | Ten => "10"
    | Jack => "J"
    | Queen => "Q"
    | King => "K"
    }

  let stringOfRank = x =>
    switch x {
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
    | King => "King"
    }

  let intOfRank = x =>
    switch x {
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
    | Ace => 14
    }

  let indexOfRank = x =>
    switch x {
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
    | King => 12
    }

  let maybeRankOfInt = x =>
    switch x {
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
    }

  let pointsOfRank = x =>
    switch x {
    | Ten => 10
    | Jack => 1
    | Queen => 2
    | King => 3
    | Ace => 4
    | _ => 0
    }

  let first = Ace
  let last = King
  let next = x =>
    switch x {
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
    | King => Ace
    }

  let listOfRanks = () => {
    let rec loop = (v, l) =>
      switch v {
      | x if x == last => Belt.List.concat(l, list{x})
      | x => loop(next(x), Belt.List.concat(l, list{x}))
      }
    loop(first, list{})
  }
}

@spice
type t = {
  rank: Rank.t,
  suit: Suit.t,
}

let stringOfCard: t => string = ({rank, suit}) =>
  Rank.stringOfRank(rank) ++ (" of " ++ Suit.toString(suit))

let stringOfMaybeCard = maybeCard =>
  switch maybeCard {
  | None => "Empty"
  | Some(card) => stringOfCard(card)
  }

let codeOfCard = card => Rank.codeOfRank(card.rank) ++ Suit.codeOfSuit(card.suit)

let codeOfMaybeCard = x =>
  switch x {
  | None => "None"
  | Some(card) => codeOfCard(card)
  }

let stringOfSpriteOffset = ({rank, suit}) => {
  let xSpacing = 195 // image pixels between each rank in a suit
  let ySpacing = 285 // image pixels between each suit
  let xOffset = Rank.indexOfRank(rank) * xSpacing + 1 // +1 offset in tandem with a 1 px solid border on the client
  let yOffset = Suit.indexOfSuit(suit) * ySpacing + 1 //   currently hides the black 1px border around cards in the deck image
  "-" ++ (string_of_int(xOffset) ++ ("px " ++ ("-" ++ (string_of_int(yOffset) ++ "px"))))
}

let sort = cards => {
  let cmp = (c1, c2) => {
    let multiplier = x =>
      switch x {
      | Suit.Spades => 0
      | Hearts => 1
      | Clubs => 2
      | Diamonds => 3
      }

    let v1 = Rank.intOfRank(c1.rank) + 13 * multiplier(c1.suit)
    let v2 = Rank.intOfRank(c2.rank) + 13 * multiplier(c2.suit)

    v1 > v2 ? -1 : v1 == v2 ? 0 : 1
  }
  List.sort(cmp, cards)
}
