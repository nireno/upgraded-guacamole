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
  let rotatedPlayerDecisions =
    Player.playersAsQuad(~startFrom=me, ())
    ->Quad.map(
        playerId => (Quad.get(playerId, players), Quad.get(playerId, rematchDecisions)),
        _,
      );
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
    {
      let decisionImage = ({ClientGame.client_identicon, client_profile_type}, decision) => {
        let identicon_style =
                  ClientSettings.dicebearTypeOfProfileType(client_profile_type);
        switch (decision) {
        | SharedGame.RematchAccepted =>
          <img src={j|https://avatars.dicebear.com/v2/$identicon_style/$client_identicon.svg|j} />
        | RematchDenied => <EmptySeatAvatarView />;
        | RematchUnknown =>
          <img
            style={ReactDOMRe.Style.make(~transform="translateX(-50%)", ())}
            src={j|https://avatars.dicebear.com/v2/$identicon_style/$client_identicon.svg|j}
          />
        };
      };

      let (bottom, right, top, left) =
        rotatedPlayerDecisions
        ->Quad.map(
            (({ClientGame.pla_profile_maybe}, decision)) =>
              switch (pla_profile_maybe) {
              | None => <EmptySeatAvatarView />
              | Some(pla_profile) => decisionImage(pla_profile, decision)
              },
            _,
          );

      <div
        className="w-2/3 my-8"
        style={ReactDOMRe.Style.make(
          ~display="grid",
          ~gridTemplateColumns="repeat(3, 1fr)",
          ~gridGap="10px",
          (),
        )}>
        <div
          className="rounded border border-gray-300 w-full rounded overflow-hidden"
          style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="1", ())}>
          top
        </div>
        <div
          className="rounded border border-gray-300 w-full rounded overflow-hidden"
          style={ReactDOMRe.Style.make(~gridColumn="1", ~gridRow="2", ())}>
          left
        </div>
        <div
          className="rounded border border-gray-300 w-full rounded overflow-hidden"
          style={ReactDOMRe.Style.make(~gridColumn="3", ~gridRow="2", ())}>
          right
        </div>
        <div
          className="rounded border border-gray-300 w-full rounded overflow-hidden"
          style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="3", ())}>
          bottom
        </div>
      </div>;
    }

    {
      SharedGame.isRematchPrimed(rematchDecisions)
        ? <div className="mt-6 text-xl text-center">
            <span> {ReasonReact.string("Rematch in: ")} </span>
            <CountdownView from={SharedGame.settings.gameStartingCountdownSeconds} />
          </div>
        : 
        ReasonReact.null
    }

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
