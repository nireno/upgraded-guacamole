module FaceDownHand = {
  [@decco]
  type t = int;

  module TransitionConf = {
    type item = int;

    [@bs.deriving abstract]
    type props = {
      [@bs.optional] left: string,
      [@bs.optional] top: string,
    }
    
    let getKey = (n) => "card-back-" ++ string_of_int(n)
  };

  module Transition = ReactSpring.MakeTransition(TransitionConf);

  [@react.component]
  let make = (~nCards) => {
      if (nCards == 0) {
        <div> {ReasonReact.string("No cards in hand")} </div>;
      } else {
        let ns:array(int) = [||];
        let n = ref(nCards);

        /* 
        `useTransition` requires an array of items for it to build the transitions.
        Face-down cards have no data except an int that says how many face down cards
        to show. So this loop makes an array of n items [n, n-1, ... , 1] that is
        consumed by `useTransition`. n is used to make the react key for each item.
        */
        while (n^ > 0) {
          Js.Array.push(n^, ns) |> ignore;
          n := n^ - 1;
        };

        let transitions =
          Transition.useTransition(
            ns,
            Transition.options(
              ~from=TransitionConf.props(~left="300px", ~top="-500px", ()),
              ~enter=TransitionConf.props(~left="0", ~top="0", ()),
              ~leave=TransitionConf.props(~left="25vw", ()),
              ~trail=100,
              (),
            ),
          );

        let makeAnimatedCard = (transition: Transition.transition) => {
          let props = transition->Transition.propsGet;

          let springStyle =
            switch (props->TransitionConf.leftGet) {
            | None => ReactDOMRe.Style.make()
            | Some(left) => ReactDOMRe.Style.make(~left, ())
            };

          let springStyle =
            switch (props->TransitionConf.topGet) {
            | None => springStyle
            | Some(top) => ReactDOMRe.(Style.combine(springStyle, Style.make(~top, ())))
            };

          <ReactSpring.AnimatedDiv
            key={transition->Transition.keyGet} className="hand-card" style=springStyle>
            <img className="card" src="./static/cards/Red_Back.svg" />
          </ReactSpring.AnimatedDiv>;
        };

        <div
          className="player-hand-row flex flex-row justify-around" >
          {
            Belt.Array.(
              map(transitions, makeAnimatedCard)
            ) |> ReasonReact.array
          }
        </div>
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
    [@bs.optional] opacity: string,
  };

  let getKey = ({card}) => Card.stringOfCard(card);
};

module HandTransition = ReactSpring.MakeTransition(HandTransitionConf);

module FaceUpHand = {
  [@decco]
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

  let checkIsCardPlayable = (handPhase, maybeLeadCard, maybeTrumpCard, cards, card) => {
    let playerIsLeader =
      switch (maybeLeadCard) {
      | None => true
      | Some(_) => false
      };

    let cardIsTrump = ({Card.suit}) =>
      switch (maybeTrumpCard) {
      | Some({Card.suit: trumpSuit}) when suit == trumpSuit => true
      | _ => false
      };

    let cardFollowsSuit = ({Card.suit}) =>
      switch (maybeLeadCard) {
      | Some({Card.suit: leadSuit}) when leadSuit == suit => true
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
    handPhase == HandPlayPhase
    && (playerIsLeader || cardIsTrump(card) || cardFollowsSuit(card) || cantFollowSuit);
  };

  let makeAnimatedCard = (transition: HandTransition.transition, clickAction) => {
    let kCard = transition->HandTransition.itemGet;
    let props = transition->HandTransition.propsGet;

    let springStyle =
      switch (props->HandTransitionConf.leftGet) {
      | None => ReactDOMRe.Style.make(~left="0", ())
      | Some(left) => ReactDOMRe.Style.make(~left, ())
      };

    let springStyle =
      switch (props->HandTransitionConf.topGet) {
      | None => springStyle
      | Some(top) => ReactDOMRe.(Style.combine(springStyle, Style.make(~top, ())))
      };

    let springStyle =
      switch (props->HandTransitionConf.opacityGet) {
      | None => springStyle
      | Some(opacity') =>
        ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
      };

    <ReactSpring.AnimatedDiv key={kCard.key} className="hand-card" style=springStyle>
      <Card ?clickAction card={kCard.card} />
    </ReactSpring.AnimatedDiv>;
  };

  [@react.component]
  let make =
      (
        ~cards: t,
        ~handPhase: phase,
        ~maybeLeadCard: option(Card.t),
        ~maybeTrumpCard: option(Card.t),
        ~sendPlayCard: Card.t => unit,
        ~onInvalidCardClick: string => unit,
      ) => {
    let keyedCards =
      List.fold_left((acc, card) => [{card, key: generateKey(acc, card)}, ...acc], [], cards);
    
    let transitions =
      HandTransition.useTransition(
        keyedCards |> Belt.List.toArray,
        HandTransition.options(
          ~from=HandTransitionConf.props(~left="300px", ~top="-500px", ~opacity="1", ()),
          ~enter=HandTransitionConf.props(~left="0", ~top="0", ()),
          ~leave=HandTransitionConf.props(~left="300px", ~opacity="0", ()),
          ~trail=100,
          (),
        ),
      );

    let checkIsCardPlayable = checkIsCardPlayable(handPhase, maybeLeadCard, maybeTrumpCard, cards);
    let makeAnimatedCard = (transition: HandTransition.transition) => {
      let kCard = transition->HandTransition.itemGet;
      let props = transition->HandTransition.propsGet;

      let springStyle =
        switch (props->HandTransitionConf.leftGet) {
        | None => ReactDOMRe.Style.make(~left="0", ())
        | Some(left) => ReactDOMRe.Style.make(~left, ())
        };

      let springStyle =
        switch (props->HandTransitionConf.topGet) {
        | None => springStyle
        | Some(top) => ReactDOMRe.(Style.combine(springStyle, Style.make(~top, ())))
        };

      let springStyle =
        switch (props->HandTransitionConf.opacityGet) {
        | None => springStyle
        | Some(opacity') =>
          ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
        };

      let clickAction = card => {
        let leadSuit =
          Js.Option.getWithDefault(Card.{rank: Rank.Ace, suit: Suit.Spades}, maybeLeadCard).suit
          |> Card.Suit.toString;

        let trumpSuit =
          Js.Option.getWithDefault(Card.{rank: Rank.Ace, suit: Suit.Spades}, maybeTrumpCard).suit
          |> Card.Suit.toString;

        let msg =
          handPhase != HandPlayPhase
            ? "Wait for your turn" : {j|You must follow suit ($leadSuit) or play trump ($trumpSuit)|j};

        checkIsCardPlayable(kCard.card)
          ? sendPlayCard(card) : onInvalidCardClick(msg);
      };

      <ReactSpring.AnimatedDiv key={kCard.key} className="hand-card pointer-events-auto" style=springStyle>
        <Card clickAction card={kCard.card} />
      </ReactSpring.AnimatedDiv>;
    };

    <>
      <div className="player-hand-row flex flex-row justify-around content-center">
        {
          let first6 = transitions->Belt.Array.slice(~offset=0, ~len=6);
          Array.map(makeAnimatedCard, first6) |> ReasonReact.array;
        }
      </div>
      <div className="player-hand-row flex flex-row justify-center content-center pointer pointer-events-none">
        {
          let second6 = transitions->Belt.Array.slice(~offset=6, ~len=6);
          Array.map(makeAnimatedCard, second6) |> ReasonReact.array;
        }
      </div>
    </>;
  };
};

[@decco] type handFacing = | FaceUpHand(FaceUpHand.t) | FaceDownHand(FaceDownHand.t);


[@react.component]
let make =
    (
      ~handFacing,
      ~handPhase: FaceUpHand.phase,
      ~maybeLeadCard: option(Card.t),
      ~maybeTrumpCard: option(Card.t),
      ~sendPlayCard: Card.t => unit,
      ~onInvalidCardClick: string => unit,
    ) => {
  switch (handFacing) {
  | FaceDownHand(n) => <FaceDownHand nCards=n />
  | FaceUpHand(cards) => <FaceUpHand cards handPhase maybeLeadCard maybeTrumpCard sendPlayCard onInvalidCardClick />
  };
};
