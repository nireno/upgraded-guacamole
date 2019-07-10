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

let getWinnerCard = (trumpCardSuit, leadCardSuit, trick) => {
  let rulingSuit: Card.Suit.t =
    Quad.exists(card => card.Card.suit == trumpCardSuit, trick)
      ? trumpCardSuit : leadCardSuit;
  Js.log("Ruling suit: " ++ Card.Suit.toString(rulingSuit));

  // which player has the highest card in the ruling suit
  Quad.withId(trick)
  |> Quad.foldLeft(
       /** Compare cards in pairs */
       ((_prevPlayerId, prevCard) as prevPlayerCard, (_playerId, card) as currPlayerCard ) =>
       if(card.Card.suit == rulingSuit && prevCard.Card.suit == rulingSuit){
        /** If both cards are in the ruling suit then compare by rank */
         Card.Rank.intOfRank(card.rank) > Card.Rank.intOfRank(prevCard.Card.rank) ? currPlayerCard : prevPlayerCard;
       }
       else {
         card.Card.suit == rulingSuit ? currPlayerCard : prevPlayerCard;
       }
     );
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
