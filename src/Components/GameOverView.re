[@bs.val]
external allfours_feedback_url: Js.Nullable.t(string) = "process.env.allfours_feedback_url";

[@react.component]
let make =
    (
      ~weScore,
      ~demScore,
      ~playAgainClick,
      ~leaveClick,
      ~me,
      ~players: Quad.t(ClientGame.playerState),
      ~rematchDecisions: Quad.t(SharedGame.rematchDecision),
    ) => {

  // Disable the play-again button if this player (me) has already chosen to rematch
  let isPlayAgainButtonDisabled =
    rematchDecisions
    ->Quad.withId
    ->Quad.exists(((playerId, decision)) => decision == SharedGame.RematchAccepted && playerId == me, _);

  let (outcomeText, outcomeImg, outcomeClass) =
    weScore >= demScore
      ? ("We win!", "./static/img/emoji_beaming.svg", "bg-green-600")
      : ("We lost...", "./static/img/emoji_crying.svg", "bg-blue-900");
  <>
    <div className={ outcomeClass ++ " text-white w-full flex flex-col items-center rounded"} >
      <div className="text-3xl"> {outcomeText |> ReasonReact.string} </div>
      <img src=outcomeImg style={ReactDOMRe.Style.make(~width="15%", ())} />
    </div>
    <div className="text-center my-6">
      {
        let decisionClass = (playerId, decision) => {
          let baseClass = "rounded mb-2 px-4 text-gray-900" ++ (playerId == me ? " py-4 border" : "");
          switch (decision) {
          | SharedGame.RematchAccepted => baseClass ++ " bg-blue-300 border-blue-500"
          | RematchDenied => baseClass ++ " bg-gray-300"
          | RematchUnknown => baseClass ++ " border border-gray-500"
          };
        };

        players
        ->Quad.withId
        ->Quad.zip(rematchDecisions)
        ->Quad.map(
            (((playerId, player), rematchDecision)) => {
              let {ClientGame.pla_name} = player;
              switch (rematchDecision) {
              | SharedGame.RematchAccepted =>
                <div key=pla_name className={decisionClass(playerId, RematchAccepted)}>
                  {ReasonReact.string({j|$pla_name ready|j})}
                </div>
              | RematchDenied =>
                <div key=pla_name className={decisionClass(playerId, RematchDenied)}>
                  {ReasonReact.string({j|$pla_name left|j})}
                </div>
              | RematchUnknown =>
                <div key=pla_name className={decisionClass(playerId, RematchUnknown)}>
                  {ReasonReact.string("Waiting for " ++ (playerId == me ? "you" : pla_name))}
                </div>
              };
            },
            _,
          )
        ->Quad.toArray
        ->ReasonReact.array;
      }
    </div>
    <div className="flex flex-row justify-around w-full">
      <button className="btn btn-grey" onClick=leaveClick>
        {ReasonReact.string("Back Home")}
      </button>
        <button className="btn btn-blue" onClick=playAgainClick disabled=isPlayAgainButtonDisabled>
          {(isPlayAgainButtonDisabled ? "Ready" : "Play Again") |> ReasonReact.string}
        </button>
    </div>
    {switch (Js.Nullable.toOption(allfours_feedback_url)) {
     | None => ReasonReact.null
     | Some(href) =>
       <div className="text-xs text-center mt-8">
         <span> {ReasonReact.string("Love it / Hate it: ")} </span>
         <a href target="_blank" rel="noopener noreferrer">
           {ReasonReact.string("click here to send feedback.")}
         </a>
       </div>
     }}
  </>;
};
