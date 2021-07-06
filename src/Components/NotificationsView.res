module NotiTransitionConf = {
  type item = Noti.t

  @deriving(abstract)
  type props = {
    @optional left: string,
    @optional top: string,
    @optional opacity: string,
  }

  let getKey = noti => noti.Noti.noti_id
}

module NotiTransition = ReactSpring.MakeTransition(NotiTransitionConf)

@react.component
let make = (~id=?, ~appRect, ~notis=list{}, ~teamId) => {
  let height = Webapi.Dom.DomRect.height(appRect)
  let fromTop = "-" ++ (Js.Float.toString(height *. 0.1) ++ "px")
  let leaveTop = fromTop

  let notiTransitions = NotiTransition.useTransition(
    notis |> Belt.List.toArray,
    NotiTransition.options(
      ~from=NotiTransitionConf.props(~left="0px", ~top=fromTop, ~opacity="0", ()),
      ~enter=NotiTransitionConf.props(~left="0px", ~top="0px", ~opacity="1", ()),
      ~leave=NotiTransitionConf.props(~left="0px", ~top=leaveTop, ~opacity="0", ()),
      ~config=ReactSpring.Common.Presets.stiff,
      ~trail=100,
      (),
    ),
  )

  let elements = Array.map((transition: NotiTransition.transition) => {
    let noti = transition->NotiTransition.itemGet
    let key = transition->NotiTransition.keyGet
    let props = transition->NotiTransition.propsGet

    let springStyle = switch props->NotiTransitionConf.leftGet {
    | None => ReactDOMRe.Style.make(~left="0", ())
    | Some(left) => ReactDOMRe.Style.make(~left, ())
    }

    let springStyle = switch props->NotiTransitionConf.topGet {
    | None => ReactDOMRe.Style.make(~top="0", ())
    | Some(top) =>
      open ReactDOMRe
      Style.combine(springStyle, ReactDOMRe.Style.make(~top, ()))
    }

    let springStyle = switch props->NotiTransitionConf.opacityGet {
    | None => springStyle
    | Some(opacity') =>
      open ReactDOMRe
      Style.combine(springStyle, Style.make(~opacity=opacity', ()))
    }

    let notificationClass = switch noti.noti_level {
    | Success => " bg-green-100 border-green-500 text-green-900 "
    | Info => " bg-teal-100 border-teal-500 text-teal-900 "
    | Warning => " bg-orange-100 border-orange-500 text-orange-900 "
    | Danger => " bg-red-100 border-red-500 text-red-900 "
    }

    let iconFillClass = switch noti.noti_level {
    | Success => " text-green-500 "
    | Info => " text-teal-500 "
    | Warning => " text-orange-500 "
    | Danger => " text-red-500 "
    }

    let icon =
      <svg
        className={"fill-current h-6 w-6 mr-4" ++ iconFillClass}
        xmlns="http://www.w3.org/2000/svg"
        viewBox="0 0 20 20">
        {switch noti.noti_level {
        | Success => <polygon id="Path-126" points="0 11 2 9 7 14 18 3 20 5 7 18" />
        | Info =>
          <path
            d="M2.93 17.07A10 10 0 1 1 17.07 2.93 10 10 0 0 1 2.93 17.07zm12.73-1.41A8 8 0 1 0 4.34 4.34a8 8 0 0 0 11.32 11.32zM9 11V9h2v6H9v-4zm0-6h2v2H9V5z"
          />
        | Warning
        | Danger =>
          <path
            d="M2.92893219,17.0710678 C6.83417511,20.9763107 13.1658249,20.9763107 17.0710678,17.0710678 C20.9763107,13.1658249 20.9763107,6.83417511 17.0710678,2.92893219 C13.1658249,-0.976310729 6.83417511,-0.976310729 2.92893219,2.92893219 C-0.976310729,6.83417511 -0.976310729,13.1658249 2.92893219,17.0710678 Z M9,5 L11,5 L11,11 L9,11 L9,5 Z M9,13 L11,13 L11,15 L9,15 L9,13 Z"
            id="Combined-Shape"
          />
        }}
      </svg>

    <ReactSpring.AnimatedDiv
      ?id
      key
      className={"notification relative border-0 border-t-2 border-solid text-sm  px-4 py-3 shadow-md" ++
      notificationClass}
      style=springStyle>
      {switch noti.noti_message {
      | Text(message) =>
        <div className="flex items-center justify-center">
          icon <div> {React.string(message)} </div>
        </div>
      | RoundSummary(summary) =>
        <RoundSummaryView
          weTeamId=teamId
          maybeTeamHigh=summary.noti_maybeTeamHigh
          maybeTeamLow=summary.noti_maybeTeamLow
          maybeTeamJack=summary.noti_maybeTeamJack
          maybeTeamGame=summary.noti_maybeTeamGame
        />
      }}
    </ReactSpring.AnimatedDiv>
  }, notiTransitions)

  <div
    className="notifications pointer-events-none absolute w-full h-full top-0 left-0 flex flex-col items-center z-30">
    {elements |> ReasonReact.array}
  </div>
}
