open AppPrelude

let getMainDivClass = x =>
  switch x {
  | North => "flex-col"
  | South => "flex-col-reverse"
  | East => "flex-row-reverse"
  | West => "flex-row"
  }

module AlignedIdenticon = {
  @react.component
  let make = (~seed, ~initials, ~style, ~zone, ~signal=?) => {
    let getClassName = zone => {
      let class_ = switch zone {
      | West => "bottom-0 left-0 "
      | North => "top-0"
      | South => "bottom-0 right-0"
      | East => "top-0 right-0"
      }

      `w-1/3 absolute opacity-80 ${class_}`
    }

    let getStyle = zone => {
      let transform = switch zone {
      | West => "translate(10%, 100%)"
      | North => "translate(-100%, 10%)"
      | South => "translate(100%, -10%)"
      | East => "translate(-10%, -100%)"
      }
      ReactDOM.Style.make(~transform, ())
    }
    <div className={getClassName(zone)} style={getStyle(zone)}>
      <PlayerIdentityView initials seed style />
      <SignalsView ?signal zone />
    </div>
  }
}

@react.component
let make = (
  ~zone,
  ~cardLeavesToZone,
  ~initials,
  ~identiconSeed,
  ~identiconStyle,
  ~maybeCard,
  ~zIndex,
  ~isDealer,
  ~isTurner,
  ~gamePhase,
  ~signal=?,
) => {
  let class_ = "player-tags flex"
  let playerTagsClass = x =>
    switch x {
    | North => `${class_} justify-center items-center horizontal`
    | South => `${class_} justify-center items-center horizontal`
    | East => `${class_} flex-col justify-center w-1/5`
    | West => `${class_} flex-col justify-center w-1/5`
    }

  <div className={"player-section w-full relative flex " ++ getMainDivClass(zone)}>
    <PlayerTagsView className={playerTagsClass(zone)} isDealer />
    <div
      className="flex-grow relative w-3/4 self-center"
      style={ReactDOM.Style.make(~zIndex=zIndex->string_of_int, ())}>
      <div className="card__placeholder relative block">
        <Svg_Decor_CardPlaceholder />
      </div>
      {isTurner ? <ActivePlayerCountdownView gamePhase /> : React.null}
      <GameBoard__CardTransition.PlayCard maybeCard enterFrom=zone leaveTo=cardLeavesToZone />
      <AlignedIdenticon initials seed=identiconSeed style=identiconStyle zone ?signal />
    </div>
  </div>
}
