open AppPrelude;

let getMainDivClass =
  fun
  | North => "flex-col"
  | South => "flex-col-reverse"
  | East => "flex-row-reverse"
  | West => "flex-row";

let getIdenticonClass = zone => {
  let class_ = switch(zone){
  | West => "bottom-0 left-0 "
  | North => "top-0"
  | South => "bottom-0 right-0"
  | East => "top-0 right-0";
  };

  {j|w-1/3 absolute opacity-80 $class_|j}
}

 let getIdenticonStyle = zone => {
   let transform =
     switch (zone) {
     | West => "translate(10%, 100%)"
     | North => "translate(-100%, 10%)"
     | South => "translate(100%, -10%)"
     | East => "translate(-10%, -100%)"
     };
   ReactDOMRe.Style.make(~transform, ());
 };

module AlignedIdenticon = {
  [@react.component]
  let make = (~seed, ~initials, ~style, ~alignmentClass, ~alignmentStyle) => {
    <div className=alignmentClass style=alignmentStyle>
      <PlayerIdentityView initials seed style />
    </div>;
  };
};

[@react.component]
let make = 
  ( ~zone
  , ~cardLeavesToZone
  , ~initials
  , ~identiconSeed
  , ~identiconStyle
  , ~maybeCard
  , ~zIndex
  , ~tags
  ) => {

    let class_ = "player-tags flex";
    let playerTagsClass =
      fun
      | North => {j|$class_ justify-center horizontal|j}
      | South => {j|$class_ justify-center horizontal|j}
      | East => {j|$class_ flex-col justify-center w-1/5|j}
      | West => {j|$class_ flex-col justify-center w-1/5|j};

    <div className={"player-section w-full relative flex " ++ getMainDivClass(zone)}>
      <PlayerTagsView
          className=playerTagsClass(zone)
          tags
      />
      <div
        className="flex-grow relative w-4/5 self-center"
        style={ReactDOMRe.Style.make(~zIndex=zIndex->string_of_int, ())}>
        <img className="card__placeholder relative block " src="./static/img/card_transparent.svg" />
        <GameBoard__CardTransition.PlayCard maybeCard enterFrom=zone leaveTo=cardLeavesToZone />

        <AlignedIdenticon
          initials
          seed=identiconSeed
          style=identiconStyle
          alignmentClass={getIdenticonClass(zone)}
          alignmentStyle={getIdenticonStyle(zone)}
        />
      </div>
    </div>;
};