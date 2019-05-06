module FaceDownHand = {
  type t = int;

  let component = ReasonReact.statelessComponent("Hand");

  let make = (~nCards, _children) => {
    ...component,
    render: _self =>
      if (nCards == 0) {
        <div> {ReasonReact.string("No cards in hand")} </div>;
      } else {
        let faceDownCards = [||];
        let n = ref(nCards);
        while (n^ > 0) {
          Js.Array.push(<div className="card card__face-down" />, faceDownCards) |> ignore;
          n := n^ - 1;
        };
        <div> ...faceDownCards </div>;
      },
  };
};


module FaceUpHand = {
  type t = list(Card.t);

  let component = ReasonReact.statelessComponent("Hand");

  let hasSuitTest = (targetSuit, cards) =>
    List.exists(({Card.suit}) => suit == targetSuit, cards);

  type phase =
    | HandWaitPhase
    | HandPlayPhase;

  type keyedCard = {
    card: Card.t,
    key: string,
  };

  /** For generating unique keys for cards even if the same card shows up twice,
  This is important when testing by duplicating decks such that the deck might have
  two cards of the same suit and rank
 */
  let generateKey = (kCards, card) => {
    let maxKey = kCards |> List.filter(kCard => kCard.card == card) |> List.length;
    Card.stringOfCard(card) ++ string_of_int(maxKey);
  };

  let make =
      (
        ~cards: t,
        ~handPhase: phase,
        ~maybeLeadCard: option(Card.t),
        ~maybeTrumpCard: option(Card.t),
        ~sendPlayCard: Card.t => unit,
        _children,
      ) => {
    let keyedCards =
      List.fold_left((acc, card) => [{card, key: generateKey(acc, card)}, ...acc], [], cards);

    let playerIsLeader =
      switch (maybeLeadCard) {
      | None => true
      | Some(_) => false
      };

    let cardIsTrump = ({Card.suit}) =>
      switch (maybeTrumpCard) {
      | Some({suit: trumpSuit}) when suit == trumpSuit => true
      | _ => false
      };

    let cardFollowsSuit = ({Card.suit}) =>
      switch (maybeLeadCard) {
      | Some({suit: leadSuit}) when leadSuit == suit => true
      | _ => false
      };

    let handHasSuitTest: Card.Suit.t => bool =
      testSuit => {
        List.exists(({Card.suit}) => suit == testSuit, cards);
      };

    let cantFollowSuit =
      switch (maybeLeadCard) {
      | Some({suit: leadSuit}) => handHasSuitTest(leadSuit) ? false : true
      | None => false
      };

    let checkIsCardPlayable = card =>
      handPhase == HandPlayPhase
      && (playerIsLeader || cardIsTrump(card) || cardFollowsSuit(card) || cantFollowSuit);

    {
      ...component,
      render: _self => {
        <div>
          {switch (keyedCards) {
           | [] => <div> {ReasonReact.string("No cards in hand")} </div>
           | _ =>
             <div>
               {List.map(
                  kCard => {
                    let isCardPlayable = checkIsCardPlayable(kCard.card);
                    Js.log(kCard.key);
                    <Card
                      key={kCard.key}
                      clickAction=?{isCardPlayable ? Some(sendPlayCard) : None}
                      card={kCard.card}
                    />;
                  },
                  keyedCards,
                )
                |> Belt.List.toArray
                |> ReasonReact.array}
             </div>
           }}
        </div>;
      },
    };
  };
};
