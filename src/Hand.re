module FaceDownHand = {
  type t = int;

  [@react.component]
  let make = (~nCards) => {
      if (nCards == 0) {
        <div> {ReasonReact.string("No cards in hand")} </div>;
      } else {
        let faceDownCards = [||];
        let n = ref(nCards);
        while (n^ > 0) {
          Js.Array.push(<img className="card" src="/static/cardsjs/cards/Red_Back.svg"/>, faceDownCards) |> ignore;
          n := n^ - 1;
        };
        <div> { ReasonReact.array(faceDownCards) } </div>;
      }
  };
};


type keyedCard = {
  card: Card.t,
  key: string,
};

module HandTransitionConf = {
  type item = keyedCard;

  [@bs.deriving abstract]
  type props = {
    [@bs.optional] left: string,
    [@bs.optional] top: string,
  }
  
  let getKey = ({card}) => Card.stringOfCard(card);
}

module HandTransition = ReactSpring.MakeTransition(HandTransitionConf);

module FaceUpHand = {
  type t = list(Card.t);

  let hasSuitTest = (targetSuit, cards) =>
    List.exists(({Card.suit}) => suit == targetSuit, cards);

  type phase =
    | HandWaitPhase
    | HandPlayPhase;


  /** For generating unique keys for cards even if the same card shows up twice,
  This is important when testing by duplicating decks such that the deck might have
  two cards of the same suit and rank
 */
  let generateKey = (kCards, card) => {
    let maxKey = kCards |> List.filter(kCard => kCard.card == card) |> List.length;
    Card.stringOfCard(card) ++ string_of_int(maxKey);
  };

  [@react.component]
  let make =
      (
        ~cards: t,
        ~handPhase: phase,
        ~maybeLeadCard: option(Card.t),
        ~maybeTrumpCard: option(Card.t),
        ~sendPlayCard: Card.t => unit,
      ) => {
    let keyedCards =
      List.fold_left((acc, card) => [{card, key: generateKey(acc, card)}, ...acc], [], cards);
    
    let transitions = HandTransition.useTransition(
        keyedCards |> Belt.List.toArray, 
        HandTransition.options(
          ~from = HandTransitionConf.props(~left="300px", ~top="-500px", ()),
          ~enter = HandTransitionConf.props(~left="0", ~top="0", ()),
          ~leave = HandTransitionConf.props(~left="25vw", ()),
          ~trail = 100,
        ));
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

        <div>
          {switch (keyedCards) {
           | [] => <div> {ReasonReact.string("No cards in hand")} </div>
           | _ =>
             <div>
               {Array.map(
                  (transition:HandTransition.transition) => {
                    let kCard = transition->HandTransition.itemGet;
                    let props = transition->HandTransition.propsGet;

                    let springStyle = switch(props->HandTransitionConf.leftGet){
                      | None => ReactDOMRe.Style.make(~left="0", ());
                      | Some(left) => ReactDOMRe.Style.make(~left, ());
                    };

                    let springStyle = switch( props->HandTransitionConf.topGet ){
                      | None => springStyle
                      | Some(top) => ReactDOMRe.(Style.combine(springStyle, Style.make(~top, ())))
                    };

                    let isCardPlayable = checkIsCardPlayable(kCard.card);
                    <Card
                      key={kCard.key}
                      style=springStyle
                      clickAction=?{isCardPlayable ? Some(sendPlayCard) : None}
                      card={kCard.card}
                    />;
                  },
                  transitions,
                )
                |> ReasonReact.array}
             </div>
           }}
        </div>;
  };
};
