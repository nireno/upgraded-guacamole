type t = list(Card.t);

let component = ReasonReact.statelessComponent("Hand");

let hasSuitTest = (targetSuit, cards) =>
  List.exists(({Card.suit}) => suit == targetSuit, cards);

let make =
    (
      ~cards: list(Card.t),
      ~isPlayerTurn: bool,
      ~leadCardSlot: option(Card.t),
      ~trumpCardSlot: option(Card.t),
      ~sendPlayCard: Card.t => unit,
      _children,
    ) => {
  let playerIsLeader =
    switch (leadCardSlot) {
    | None => true
    | Some(_) => false
    };

  let cardIsTrump = ({Card.suit}) =>
    switch (trumpCardSlot) {
    | Some({suit: trumpSuit}) when suit == trumpSuit => true
    | _ => false
    };

  let cardFollowsSuit = ({Card.suit}) =>
    switch (leadCardSlot) {
    | Some({suit: leadSuit}) when leadSuit == suit => true
    | _ => false
    };

  let handHasSuitTest: Card.Suit.t => bool =
    testSuit => {
      List.exists(({Card.suit}) => suit == testSuit, cards);
    };

  let cantFollowSuit =
    switch (leadCardSlot) {
    | Some({suit: leadSuit}) => handHasSuitTest(leadSuit) ? false : true
    | None => false
    };

  let checkIsCardPlayable = card =>
    isPlayerTurn
    && (
      playerIsLeader
      || cardIsTrump(card)
      || cardFollowsSuit(card)
      || cantFollowSuit
    );

  {
    ...component,
    render: _self => {
      <ul>
        {List.map(
           c => {
             let isCardPlayable = checkIsCardPlayable(c);
             <Card
               key={Card.stringOfCard(c)}
               clickAction=?{isCardPlayable ? Some(sendPlayCard) : None}
               card=c
             />;
           },
           cards,
         )
         |> Belt.List.toArray
         |> ReasonReact.array}
      </ul>;
    },
  };
};
