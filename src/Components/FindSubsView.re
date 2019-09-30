[@react.component]
let make = (~emptySeatCount as n, ~onLeaveClick) => {
  let players = Grammar.byNumber(n, "player");
  let n = string_of_int(n);
  <div className="text-center">
    <div> {ReasonReact.string({j|$n $players disconnected|j})} </div>
    <div> {ReasonReact.string({j|Please wait while I find substitutes...|j})} </div>
    <button className="btn btn-blue mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Leave Game")}
    </button>
  </div>;
};
