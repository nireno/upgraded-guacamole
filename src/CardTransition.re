type direction = | North | South | East | West;

module PlayCard = {
  module PlayCardTransitionConf = {
    type item = Card.t;

    [@bs.deriving abstract]
    type props = {
      [@bs.optional] left: string,
      [@bs.optional] top: string,
      [@bs.optional] opacity: string,
    };

    /** This is blank cause I can't remember why i needed it in the first place */
    let getKey = _ => "";
  };

  module PlayCardTransition = ReactSpring.MakeTransition(PlayCardTransitionConf);

  [@react.component]
  let make = (~maybeCard, ~enterFrom, ~leaveTo) => {
    let cards =
      switch (maybeCard) {
      | None => [||]
      | Some(card) => [|card|]
      };

    let (enterLeft, enterTop) =
      switch (enterFrom) {
      | North => ("0", "-300px")
      | South => ("0", "300px")
      | East => ("300px", "0")
      | West => ("-300px", "0")
      };

    let (leaveLeft, leaveTop) =
      switch (leaveTo) {
      | North => ("0", "-300px")
      | South => ("0", "300px")
      | East => ("300px", "0")
      | West => ("-300px", "0")
      };

    let transitions =
      PlayCardTransition.useTransition(
        cards,
        PlayCardTransition.options(
          ~from=PlayCardTransitionConf.props(~left=enterLeft, ~top=enterTop, ~opacity="0", ()),
          ~enter=PlayCardTransitionConf.props(~left="0", ~top="0", ~opacity="1", ()),
          ~leave=PlayCardTransitionConf.props(~left=leaveLeft, ~top=leaveTop, ~opacity="0", ()),
          ~trail=100,
        ),
      );
    Array.map(
      (transition: PlayCardTransition.transition) => {
        let card = transition->PlayCardTransition.itemGet;
        let key = Card.stringOfCard(card);
        let props = transition->PlayCardTransition.propsGet;
        // let key = transition->SouthCardTransition.keyGet;

        let springStyle =
          switch (props->PlayCardTransitionConf.leftGet) {
          | None => ReactDOMRe.Style.make(~left="0", ())
          | Some(left) => ReactDOMRe.Style.make(~left, ())
          };

        let springStyle =
          switch (props->PlayCardTransitionConf.topGet) {
          | None => ReactDOMRe.Style.make(~top="0", ())
          | Some(top) => ReactDOMRe.(Style.combine(springStyle, ReactDOMRe.Style.make(~top, ())))
          };

        let springStyle =
          switch (props->PlayCardTransitionConf.opacityGet) {
          | None => springStyle
          | Some(opacity') =>
            ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
          };
        <ReactSpring.AnimatedDiv key className="animated-card" style=springStyle>
          <Card card />
        </ReactSpring.AnimatedDiv>;
      },
      transitions,
    )
    |> ReasonReact.array;
  };
};
