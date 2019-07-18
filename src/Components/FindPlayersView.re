[@react.component]
let make = (~n, ~canSub) => {
  let players = Grammar.byNumber(n, "player");
  let n = string_of_int(n);
  
  <div className="text-center">
    <div> {ReasonReact.string({j|Finding $n more $players. Please wait...|j})} </div>
    {
      canSub ? <button>{ReasonReact.string("Join a game already in progress")}</button> : ReasonReact.null
    }
  </div>;
};
