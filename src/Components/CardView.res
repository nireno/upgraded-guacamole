@react.component
let make = (~card, ~clickAction=?, ~style=?) => {
  let (onClick, isClickable) = switch clickAction {
  | None => (None, false)
  | Some(handleClick) => (Some(_event => handleClick(card)), true)
  }
  <ReactSpring.AnimatedDiv
    ?style className={"card" ++ (isClickable ? " cursor-pointer" : "")} ?onClick>
    {switch card {
    | {Card.rank: Two, suit: Clubs} => <Svg_Card_2C />
    | {Card.rank: Two, suit: Diamonds} => <Svg_Card_2D />
    | {Card.rank: Two, suit: Hearts} => <Svg_Card_2H />
    | {Card.rank: Two, suit: Spades} => <Svg_Card_2S />
    | {Card.rank: Three, suit: Diamonds} => <Svg_Card_3D />
    | {Card.rank: Three, suit: Hearts} => <Svg_Card_3H />
    | {Card.rank: Three, suit: Spades} => <Svg_Card_3S />
    | {Card.rank: Three, suit: Clubs} => <Svg_Card_3C />
    | {Card.rank: Four, suit: Clubs} => <Svg_Card_4C />
    | {Card.rank: Four, suit: Diamonds} => <Svg_Card_4D />
    | {Card.rank: Four, suit: Hearts} => <Svg_Card_4H />
    | {Card.rank: Four, suit: Spades} => <Svg_Card_4S />
    | {Card.rank: Five, suit: Clubs} => <Svg_Card_5C />
    | {Card.rank: Five, suit: Diamonds} => <Svg_Card_5D />
    | {Card.rank: Five, suit: Hearts} => <Svg_Card_5H />
    | {Card.rank: Five, suit: Spades} => <Svg_Card_5S />
    | {Card.rank: Six, suit: Clubs} => <Svg_Card_6C />
    | {Card.rank: Six, suit: Diamonds} => <Svg_Card_6D />
    | {Card.rank: Six, suit: Hearts} => <Svg_Card_6H />
    | {Card.rank: Six, suit: Spades} => <Svg_Card_6S />
    | {Card.rank: Seven, suit: Clubs} => <Svg_Card_7C />
    | {Card.rank: Seven, suit: Diamonds} => <Svg_Card_7D />
    | {Card.rank: Seven, suit: Hearts} => <Svg_Card_7H />
    | {Card.rank: Seven, suit: Spades} => <Svg_Card_7S />
    | {Card.rank: Eight, suit: Clubs} => <Svg_Card_8C />
    | {Card.rank: Eight, suit: Diamonds} => <Svg_Card_8D />
    | {Card.rank: Eight, suit: Hearts} => <Svg_Card_8H />
    | {Card.rank: Eight, suit: Spades} => <Svg_Card_8S />
    | {Card.rank: Nine, suit: Clubs} => <Svg_Card_9C />
    | {Card.rank: Nine, suit: Diamonds} => <Svg_Card_9D />
    | {Card.rank: Nine, suit: Hearts} => <Svg_Card_9H />
    | {Card.rank: Nine, suit: Spades} => <Svg_Card_9S />
    | {Card.rank: Ten, suit: Clubs} => <Svg_Card_10C />
    | {Card.rank: Ten, suit: Diamonds} => <Svg_Card_10D />
    | {Card.rank: Ten, suit: Hearts} => <Svg_Card_10H />
    | {Card.rank: Ten, suit: Spades} => <Svg_Card_10S />
    | {Card.rank: Jack, suit: Clubs} => <Svg_Card_JC />
    | {Card.rank: Jack, suit: Diamonds} => <Svg_Card_JD />
    | {Card.rank: Jack, suit: Hearts} => <Svg_Card_JH />
    | {Card.rank: Jack, suit: Spades} => <Svg_Card_JS />
    | {Card.rank: Queen, suit: Clubs} => <Svg_Card_QC />
    | {Card.rank: Queen, suit: Diamonds} => <Svg_Card_QD />
    | {Card.rank: Queen, suit: Hearts} => <Svg_Card_QH />
    | {Card.rank: Queen, suit: Spades} => <Svg_Card_QS />
    | {Card.rank: King, suit: Clubs} => <Svg_Card_KC />
    | {Card.rank: King, suit: Diamonds} => <Svg_Card_KD />
    | {Card.rank: King, suit: Hearts} => <Svg_Card_KH />
    | {Card.rank: King, suit: Spades} => <Svg_Card_KS />
    | {Card.rank: Ace, suit: Clubs} => <Svg_Card_AC />
    | {Card.rank: Ace, suit: Diamonds} => <Svg_Card_AD />
    | {Card.rank: Ace, suit: Hearts} => <Svg_Card_AH />
    | {Card.rank: Ace, suit: Spades} => <Svg_Card_AS />
    }}
  </ReactSpring.AnimatedDiv>
}
