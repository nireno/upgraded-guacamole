type t = list(Card.t);

let component = ReasonReact.statelessComponent("Hand");

let make = (~cards, ~onCardSelected, _children) => {
  {
    ...component,
    render: ({state: _state}) => {
      <ul>
        {List.map(
           c => <Card key={Card.stringOfCard(c)} onCardSelected card=c />,
           cards,
         )
         |> Belt.List.toArray
         |> ReasonReact.array}
      </ul>;
    },
  };
};
