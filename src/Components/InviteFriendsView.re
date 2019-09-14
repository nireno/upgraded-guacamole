[@react.component]
let make =
    (
      ~me,
      ~onLeaveClick,
      ~inviteCode,
      ~emptySeatCount,
      ~players,
      ~onGoPublicClick,
      ~onSelectPartnerClick,
      ~onStartGameClick,
      ~private_game_master,
    ) => {
  let rotatedPlayers =  
    Player.playersAsQuad(~startFrom=me, ())
    ->Quad.map(playerId => Quad.get(playerId, players), _);

  let initialCopyText = "copy invite code";
  let (copyText, updateCopyText) = React.useState(() => initialCopyText);
  let friends = Grammar.byNumber(emptySeatCount, "friend");
  let emptySeatText = string_of_int(emptySeatCount);

  /* To aid in cleaning up remnants of `useEffect` when the component unmounts 
     There must be better ways to do this. See My__ReactHooks for an example wrt setInterval */
  let maybeCopySuccessTimeout = ref(None);
  let maybeClipboard = ref(None);

  let onCopyFailure = _event => {
    updateCopyText(_ => "Sorry. Please manually copy the invite code.");
  };

  let rec onCopySuccess = _event => {
    switch (maybeClipboard^) {
    | None => ()
    | Some(clipboard) =>
      updateCopyText(_ => "Copied!");
      clipboard->Clipboard.off("success");
      clipboard->Clipboard.off("error");

      let timeoutId =
        Js.Global.setTimeout(
          () => {
            updateCopyText(_ => initialCopyText);
            clipboard->Clipboard.on("success", onCopySuccess);
            clipboard->Clipboard.on("error", onCopyFailure);
          },
          2000,
        );
      maybeCopySuccessTimeout := Some(timeoutId);
    };
  };


  React.useEffect0(() => {
    let stringOfTrigger = _ => inviteCode;
    let clipboard = Clipboard.make(".copy-invite-code", Clipboard.options(~text=stringOfTrigger));
    clipboard->Clipboard.on("success", onCopySuccess);
    clipboard->Clipboard.on("error", onCopyFailure);
    maybeClipboard := Some(clipboard);
    Some(
      () => {
        switch (maybeCopySuccessTimeout^) {
        | None => ()
        | Some(timeoutId) => Js.Global.clearTimeout(timeoutId)
        };
        clipboard->Clipboard.off("success");
        clipboard->Clipboard.off("error");
      },
    );
  });
  <>
   <div className="text-center text-xl">{ReasonReact.string({j|Invite friends|j})}</div>
        {
        let (bottom, right, top, left) = 
          rotatedPlayers
          ->Quad.map(
              ({ClientGame.pla_profile_maybe}) => {
                switch (pla_profile_maybe) {
                | None =>
                  <EmptySeatAvatarView />
                | Some({client_identicon, client_profile_type}) =>
                  let identicon_style = ClientSettings.dicebearTypeOfProfileType(client_profile_type);
                  <img
                    src={j|https://avatars.dicebear.com/v2/$identicon_style/$client_identicon.svg|j}
                    className="rounded border border-gray-300 w-full rounded"
                  />;
                };
              },
              _,
            );

        <div
          className="w-1/2 my-8"
          style={ReactDOMRe.Style.make(
            ~display="grid",
            ~gridTemplateColumns="repeat(3, 1fr)",
            ~gridGap="10px",
            (),
          )}>
          <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="1", ())}> top </div>
          <div style={ReactDOMRe.Style.make(~gridColumn="1", ~gridRow="2", ())}> left </div>
          {
            me == private_game_master
              ? <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="2", ())}>
                  <img
                    style={ReactDOMRe.Style.make(~opacity=emptySeatCount == 3 ? "0.5" : "1.0", ())}
                    src="./static/img/rotate_tri.svg"
                    className={
                      "w-full border border-b-4 border-blue-800 bg-blue-500 p-2 rounded-full"
                      ++ (emptySeatCount == 3 ? " cursor-not-allowed" : " cursor-pointer")
                    }
                    onClick=?{emptySeatCount == 3 ? None : Some(onSelectPartnerClick)}
                  />
                </div>
              : ReasonReact.null;
          }
          <div style={ReactDOMRe.Style.make(~gridColumn="3", ~gridRow="2", ())}> right </div>
          <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="3", ())}> bottom </div>
        </div>;
        }
  <div className="text-center">
    <div className="text-xl">
      {ReasonReact.string({j|Tell your friends to join a private game with this invite code:|j})}
    </div>
    <div className="mt-6 mb-1"> {ReasonReact.string({j|$inviteCode|j})} </div>
    <a
      className="copy-invite-code block text-base text-blue-700 hover:text-blue-500 underline cursor-pointer">
      {ReasonReact.string(copyText)}
    </a>
    {
      emptySeatCount == 0 
        ? <div>
           <span>{ ReasonReact.string("Game starting in: ") }</span>
           <CountdownView from=SharedGame.settings.gameStartingCountdownSeconds />
          </div>
        :
    <div className="text-xl mt-6">
      {ReasonReact.string({j|Waiting for $emptySeatText more $friends...|j})}
    </div>
    }
    <div className="flex justify-around">
    <button className="btn btn-grey mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Cancel")}
    </button>
    {
      me == private_game_master
        ? emptySeatCount == 0
            ? <button className="btn btn-blue mt-4" onClick=onStartGameClick>
                {ReasonReact.string("Start Now")}
              </button>
            : <button className="btn btn-blue mt-4" onClick=onGoPublicClick>
                {ReasonReact.string("Go Public")}
              </button>
        : ReasonReact.null;
    }
    </div>
  </div>
  </>
};
