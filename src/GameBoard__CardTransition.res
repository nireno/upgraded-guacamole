open AppPrelude

let directionToString = x =>
  switch x {
  | North => "north"
  | South => "south"
  | East => "east"
  | West => "west"
  }

module PlayCard = {
  module PlayCardTransitionConf = {
    type item = Card.t

    @deriving(abstract)
    type props = {
      @optional left: string,
      @optional top: string,
      @optional opacity: string,
    }

    @ocaml.doc(" This is blank cause I can't remember why i needed it in the first place ")
    let getKey = _ => ""
  }

  module PlayCardTransition = ReactSpring.MakeTransition(PlayCardTransitionConf)

  @react.component
  let make = (~maybeCard, ~enterFrom, ~leaveTo) => {
    let cards = switch maybeCard {
    | None => []
    | Some(card) => [card]
    }

    let (enterLeft, enterTop) = switch enterFrom {
    | North => ("0px", "-300px")
    | South => ("0px", "300px")
    | East => ("300px", "0px")
    | West => ("-300px", "0px")
    }

    let (leaveLeft, leaveTop) = switch leaveTo {
    | North => ("0px", "-300px")
    | South => ("0px", "300px")
    | East => ("300px", "0px")
    | West => ("-300px", "0px")
    }

    let transitions = PlayCardTransition.useTransition(
      cards,
      PlayCardTransition.options(
        ~from=PlayCardTransitionConf.props(~left=enterLeft, ~top=enterTop, ~opacity="0", ()),
        ~enter=PlayCardTransitionConf.props(~left="0", ~top="0", ~opacity="1", ()),
        ~leave=PlayCardTransitionConf.props(~left=leaveLeft, ~top=leaveTop, ~opacity="0", ()),
        ~trail=100,
        (),
      ),
    )
    Array.map((transition: PlayCardTransition.transition) => {
      let card = transition->PlayCardTransition.itemGet
      let key = Card.stringOfCard(card)
      let props = transition->PlayCardTransition.propsGet

      let springStyle = switch props->PlayCardTransitionConf.leftGet {
      | None => ReactDOMRe.Style.make(~left="0", ())
      | Some(left) => ReactDOMRe.Style.make(~left, ())
      }

      let springStyle = switch props->PlayCardTransitionConf.topGet {
      | None => ReactDOMRe.Style.make(~top="0", ())
      | Some(top) =>
        open ReactDOMRe
        Style.combine(springStyle, ReactDOMRe.Style.make(~top, ()))
      }

      let springStyle = switch props->PlayCardTransitionConf.opacityGet {
      | None => springStyle
      | Some(opacity') =>
        open ReactDOMRe
        Style.combine(springStyle, Style.make(~opacity=opacity', ()))
      }
      <ReactSpring.AnimatedDiv key className="animated-card" style=springStyle>
        <Card card />
      </ReactSpring.AnimatedDiv>
    }, transitions) |> ReasonReact.array
  }
}
