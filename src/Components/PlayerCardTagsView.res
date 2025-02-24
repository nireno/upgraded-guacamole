@react.component
let make = (~cards, ~className=?, ~style=?) =>
  <div ?className ?style> {React.array(Belt.List.toArray(List.map(card => {
          let suitCode = Card.Suit.codeOfSuit(card.Card.suit)
          let rankCode = Card.Rank.codeOfRank(card.rank)
          <img
            key={`${rankCode}${suitCode}`}
            className="border border-solid border-black bg-white"
            src={`./static/card_icons/${rankCode}${suitCode}.svg`}
            style={ReactDOM.Style.make(~width="12%", ~display="inline-block", ())}
          />
        }, cards)))} </div>
