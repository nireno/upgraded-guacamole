open AppPrelude;

let classNameOfZone =
  fun
  | North => "board-card-north"
  | South => "board-card-south"
  | East => "board-card-east"
  | West => "board-card-west";

[@react.component]
let make = 
  ( ~zone
  , ~cardLeavesToZone
  , ~initials
  , ~identiconSeed
  , ~identiconStyle
  , ~maybeCard
  , ~zIndex
  ) => {
    let className = classNameOfZone(zone);

    <div
      className={j|"board-card $className flex-shrink-0"|j}
      style={ReactDOMRe.Style.make(~zIndex=string_of_int(zIndex), ())}>
      <div
        className="w-1/3 absolute bottom-0 left-0 opacity-80"
        style={ReactDOMRe.Style.make(~transform="translate(0, 100%)", ())}>
        <PlayerIdentityView
          initials=initials
          seed=identiconSeed
          style=identiconStyle
        />
      </div>
      <img className="card__placeholder relative block" src="./static/img/card_transparent.svg" />
      <GameBoard__CardTransition.PlayCard
        maybeCard
        enterFrom=zone
        leaveTo=cardLeavesToZone
      />
    </div>;
};