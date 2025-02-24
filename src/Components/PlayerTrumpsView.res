@react.component
let make = (~suit, ~n=0, ~style=?, ~className=?) => {
  let suitName = Js.String.toLowerCase(Card.Suit.toString(suit))

  let className = switch className {
  | None => ""
  | Some(className) => className
  }

  <div ?style className={className ++ " flex flex-row justify-end items-center"}>
    <img className="w-1/6 mx-1" src={`./static/card_icons/${suitName}_bubble.min.svg`} />
    <span className="text-red mx-1"> {React.string("x")} </span>
    <span className="text-red mx-1"> {React.string(string_of_int(n))} </span>
  </div>
}
