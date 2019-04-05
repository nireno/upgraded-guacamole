type t = list(Card.t);

let component = ReasonReact.statelessComponent("Hand");

let hasSuitTest = (targetSuit, cards) =>
  List.exists(((_, suit)) => suit == targetSuit, cards);

let make =
    (
      ~cards,
      ~isPlayerTurn,
      ~leadCardSlot,
      ~trumpCardSlot,
      ~sendPlayCard,
      _children,
    ) => {
  let playerIsLeader =
    switch (leadCardSlot) {
    | None => true
    | Some(_) => false
    };
  let cardIsTrump = ((_, suit)) =>
    switch (trumpCardSlot) {
    | Some((_, trumpSuit)) when suit == trumpSuit => true
    | _ => false
    };

  let cardFollowsSuit = ((_, suit)) =>
    switch (leadCardSlot) {
    | Some((_, leadSuit)) when leadSuit == suit => true
    | _ => false
    };

  let handHasSuitTest = testSuit => {
    List.exists(((_, suit)) => suit == testSuit, cards);
  };

  let cantFollowSuit =
    switch (leadCardSlot) {
    | Some((_, leadSuit)) => handHasSuitTest(leadSuit) ? false : true
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
