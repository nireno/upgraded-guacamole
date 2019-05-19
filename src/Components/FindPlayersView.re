[@react.component]
let make = (~n) => {
  let players = Grammar.byNumber(n, "player");
  let n = string_of_int(n);
  <div className="text-center text-white bg-orange-500 my-5 p-2">
    {ReasonReact.string({j|Finding $n more $players. Please wait...|j})}
  </div>;
};
