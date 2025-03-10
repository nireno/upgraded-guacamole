let suits = Card.Suit.listOfSuits()
let ranks = Card.Rank.listOfRanks()

@spice
type t = list<Card.t>

let make = () => List.flatten(List.map(suit => List.map(rank => {
        open Card
        {rank, suit}
      }, ranks), suits))

// https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml

let shuffle = Belt.List.shuffle

let deal = (n, d) => (
  Js.Option.getWithDefault(list{}, Belt.List.take(d, n)),
  Js.Option.getWithDefault(d, Belt.List.drop(d, n)),
)

let deal1Exn = d => (List.hd(d), List.tl(d))

let testFinalTrumpTransitionToRedeal = {
  open Card
  list{
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
  }
}

let testFinalTrumpTransitionToPlayerTurn = {
  open Card
  list{
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
  }
}

let testFinalTrumpTransitionToGameOver = {
  open Card
  list{
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
  }
}
