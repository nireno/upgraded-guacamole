@@ocaml.doc(" One trick is completed when each player play's one card on the board ")

@decco
type t = (Card.t, Card.t, Card.t, Card.t)

let codeOfTrick = trick => {
  let (card1Code, card2Code, card3Code, card4Code) = Quad.map(card => Card.codeOfCard(card), trick)
  j`($card1Code, $card2Code, $card3Code, $card4Code)`
}

let stringOfTrick = r => {
  let stringOfPlayerCard = ((playerId, card)) =>
    Player.stringOfId(playerId) ++ (": " ++ Card.stringOfCard(card))

  Quad.toDict(r)
  |> List.map(stringOfPlayerCard)
  |> List.fold_left((acc, s) => acc ++ (" " ++ s), "")
}

let getWinnerCard = (trumpCardSuit, leadCardSuit, trick) => {
  let rulingSuit: Card.Suit.t = Quad.exists(card => card.Card.suit == trumpCardSuit, trick)
    ? trumpCardSuit
    : leadCardSuit

  // which player has the highest card in the ruling suit
  Quad.withId(trick) |> Quad.foldLeft(@ocaml.doc(" Compare cards in pairs ")
  ((_prevPlayerId, prevCard) as prevPlayerCard, (_playerId, card) as currPlayerCard) =>
    if card.Card.suit == rulingSuit && prevCard.Card.suit == rulingSuit {
      @ocaml.doc(" If both cards are in the ruling suit then compare by rank ")
      Card.Rank.intOfRank(card.rank) >
      Card.Rank.intOfRank(prevCard.Card.rank)
        ? currPlayerCard
        : prevPlayerCard
    } else if card.Card.suit == rulingSuit {
      currPlayerCard
    } else {
      prevPlayerCard
    }
  )
}

@react.component
let make = (~trick) => {
  let (c1, c2, c3, c4) = trick
  <ul>
    <li> {React.string("P1: " ++ Card.stringOfCard(c1))} </li>
    <li> {React.string("P2: " ++ Card.stringOfCard(c2))} </li>
    <li> {React.string("P3: " ++ Card.stringOfCard(c3))} </li>
    <li> {React.string("P4: " ++ Card.stringOfCard(c4))} </li>
  </ul>
}

let getValue = trick =>
  Quad.toList(trick) |> List.fold_left(
    (acc, {Card.rank: rank}) => acc + Card.Rank.pointsOfRank(rank),
    0,
  )
