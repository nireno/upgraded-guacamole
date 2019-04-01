type hand = list(Card.t);
type lift = list((Card.t, Card.t, Card.t, Card.t));

type id =
  | P1
  | P2
  | P3
  | P4;

let firstPlayer = P1;
let lastPlayer = P4;

let nextPlayer =
  fun
  | P1 => P2
  | P2 => P3
  | P3 => P4
  | P4 => P1;

let toString =
  fun
  | P1 => "Player 1"
  | P2 => "Player 2"
  | P3 => "Player 3"
  | P4 => "Player 4";
