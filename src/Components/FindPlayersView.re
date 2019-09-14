[@react.component]
let make = (~emptySeatCount, ~canSub, ~onLeaveClick, ~onSubClick) => {
  let playersText = Grammar.byNumber(emptySeatCount, "player");
  let emptySeatText = string_of_int(emptySeatCount);

  <div className="text-center">
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
       {
         emptySeatCount == 0
           ? <div>
               <span> {ReasonReact.string("Game starting in: ")} </span>
               <CountdownView from={SharedGame.settings.gameStartingCountdownSeconds} />
             </div>
           : <div className="text-xl mt-6">
               {ReasonReact.string({j|Finding $emptySeatText more $playersText. Please wait...|j})}
             </div>;
       }
    <button className="btn btn-blue mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Cancel")}
    </button>
  </div>;
};
