[@react.component]
let make = (~cards, ~className=?, ~style=?) => {
  <div ?className ?style>
    {List.map(
       card => {
         let suitCode = Card.Suit.codeOfSuit(card.Card.suit);
         let rankCode = Card.Rank.codeOfRank(card.rank);
         <img
           key={j|$rankCode$suitCode|j}
           className="border border-solid border-black bg-white"
           src={j|./static/card_icons/$rankCode$suitCode.svg|j}
           style={ReactDOMRe.Style.make(~width="12%", ~display="inline-block", ())}
         />;
       },
       cards,
     )
     |> Belt.List.toArray
     |> ReasonReact.array}
  </div>;
};
