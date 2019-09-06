[@react.component]
let make = (~n, ~canSub, ~onLeaveClick, ~onSubClick) => {
  let playersText = Grammar.byNumber(n, "player");
  let n = string_of_int(n);

  <div className="text-center">
    <div> {ReasonReact.string({j|Finding $n more $playersText. Please wait...|j})} </div>
    {canSub
       ? <div className="bg-green-600 text-white text-base m-4 p-4 rounded">
           <div>
             {ReasonReact.string(
                "I found a game that needs substitute players. Would you like to join that game instead?",
              )}
           </div>
           <button className="btn btn-grey mt-4" onClick=onSubClick>
             {ReasonReact.string("Join as substitute player")}
           </button>
         </div>
       : ReasonReact.null}
    <button className="btn btn-blue mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Cancel")}
    </button>
  </div>;
};
