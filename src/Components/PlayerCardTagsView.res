@react.component
let make = (~cards, ~className=?, ~style=?) =>
  <div ?className ?style> {React.array(Belt.List.toArray(List.map(card => {
          <div
            className="border border-solid border-black bg-white"
            style={ReactDOM.Style.make(~width="12%", ~display="inline-block", ())}>
            {switch card {
            | {Card.rank: Ten, suit: Clubs} => <Svg_CardIcon_10C />
            | {Card.rank: Ten, suit: Diamonds} => <Svg_CardIcon_10D />
            | {Card.rank: Ten, suit: Hearts} => <Svg_CardIcon_10H />
            | {Card.rank: Ten, suit: Spades} => <Svg_CardIcon_10S />
            | {Card.rank: Jack, suit: Clubs} => <Svg_CardIcon_JC />
            | {Card.rank: Jack, suit: Diamonds} => <Svg_CardIcon_JD />
            | {Card.rank: Jack, suit: Hearts} => <Svg_CardIcon_JH />
            | {Card.rank: Jack, suit: Spades} => <Svg_CardIcon_JS />
            | {Card.rank: Queen, suit: Clubs} => <Svg_CardIcon_QC />
            | {Card.rank: Queen, suit: Diamonds} => <Svg_CardIcon_QD />
            | {Card.rank: Queen, suit: Hearts} => <Svg_CardIcon_QH />
            | {Card.rank: Queen, suit: Spades} => <Svg_CardIcon_QS />
            | {Card.rank: King, suit: Clubs} => <Svg_CardIcon_KC />
            | {Card.rank: King, suit: Diamonds} => <Svg_CardIcon_KD />
            | {Card.rank: King, suit: Hearts} => <Svg_CardIcon_KH />
            | {Card.rank: King, suit: Spades} => <Svg_CardIcon_KS />
            | {Card.rank: Ace, suit: Clubs} => <Svg_CardIcon_AC />
            | {Card.rank: Ace, suit: Diamonds} => <Svg_CardIcon_AD />
            | {Card.rank: Ace, suit: Hearts} => <Svg_CardIcon_AH />
            | {Card.rank: Ace, suit: Spades} => <Svg_CardIcon_AS />
            | _ => failwith(`No icon found for card ${card->Card.stringOfCard}`)
            }}
          </div>
        }, cards)))} </div>
