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

  let (outcomeText, outcomeImg) =
    weScore >= demScore
      ? ("We win!", "./static/img/emoji_beaming.svg")
      : ("We lost...", "./static/img/emoji_crying.svg");
  <>
    <div> {outcomeText |> ReasonReact.string} </div>
    <img src=outcomeImg style={ReactDOMRe.Style.make(~width="15%", ())} />
    <div>
      {
        Quad.zip(players, rematchDecisions)
        ->Quad.toArray
        ->Belt.Array.map((({ClientGame.pla_name}, rematchDecision)) =>
            switch (rematchDecision) {
            | RematchAccepted =>
              <div> {ReasonReact.string({j|$pla_name is ready for a rematch|j})} </div>
            | RematchDenied => <div> {ReasonReact.string({j|$pla_name has left the game|j})} </div>
            | RematchUnknown => ReasonReact.null
            }
          )
        |> ReasonReact.array;
      }
    </div>
    <div className="flex flex-row justify-around w-full">
      <button className="btn btn-grey" onClick=leaveClick>
        {ReasonReact.string("Back Home")}
      </button>
        <button className="btn btn-blue" onClick=playAgainClick disabled=isPlayAgainButtonDisabled>
          {(isPlayAgainButtonDisabled ? "Please Wait..." : "PlayAgain") |> ReasonReact.string}
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
