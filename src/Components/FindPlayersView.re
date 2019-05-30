[@react.component]
let make = (~n) => {
  let players = Grammar.byNumber(n, "player");
  let n = string_of_int(n);
  <div className="text-center">
    {ReasonReact.string({j|Finding $n more $players. Please wait...|j})}
  </div>;
};
