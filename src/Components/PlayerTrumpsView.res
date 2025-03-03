@react.component
let make = (~suit: Card.Suit.t, ~n=0, ~style=?, ~className=?) => {
  let className = switch className {
  | None => ""
  | Some(className) => className
  }

  <div ?style className={className ++ " flex flex-row justify-end items-center"}>
    <div className="w-1/6 mx-1">
      {switch suit {
      | Hearts => <Svg_CardIcon_HeartsBubble />
      | Diamonds => <Svg_CardIcon_DiamondsBubble />
      | Clubs => <Svg_CardIcon_ClubsBubble />
      | Spades => <Svg_CardIcon_SpadesBubble />
      }}
    </div>
    <span className="text-red mx-1"> {React.string("x")} </span>
    <span className="text-red mx-1"> {React.string(string_of_int(n))} </span>
  </div>
}
