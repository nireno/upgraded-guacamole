[@react.component]
let make = (~me, ~onLeaveClick, ~inviteCode, ~n, ~players, ~onGoPublicClick, ~onSelectPartnerClick) => {
  let rotatedPlayersWithId =  
    Player.playersAsQuad(~startFrom=me, ())
    ->Quad.map(playerId => (playerId, Quad.get(playerId, players)), _);

  let initialCopyText = "copy invite code";
  let (copyText, updateCopyText) = React.useState(() => initialCopyText);
  let friends = Grammar.byNumber(n, "friend");
  let n = string_of_int(n);


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
        // The identicons of your current oppenents are clickable to allow you to switch one of them
        // with your current partner.
        let isIdenticonClickable = (~mySeatId, ~playerSeatId) =>
          mySeatId == Quad.N1 && (playerSeatId == Quad.nextId(mySeatId) || playerSeatId == Quad.prevId(mySeatId));

        let (bottom, right, top, left) = 
          rotatedPlayersWithId
          ->Quad.map(
              ((playerId, {ClientGame.pla_profile_maybe})) => {
                let isIdenticonClickable = isIdenticonClickable(~mySeatId=me, ~playerSeatId=playerId);
                switch (pla_profile_maybe) {
                | None =>
                  <img src="./static/img/frame50x50.svg" className="w-full border border-gray-300 rounded" />
                | Some({client_identicon}) =>
                  <img
                    src={j|https://avatars.dicebear.com/v2/jdenticon/$client_identicon.svg|j}
                    className={
                      "rounded border border-gray-300 p-2 w-full rounded"
                      ++ (isIdenticonClickable ? " cursor-pointer" : "")
                    }
                    onClick=?{isIdenticonClickable ? Some(_event => onSelectPartnerClick(playerId)) : None}
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
          <div style={ReactDOMRe.Style.make(~gridColumn="2/3", ~gridRow="1", ())}> top </div>
          <div style={ReactDOMRe.Style.make(~gridColumn="1", ~gridRow="2", ())}> left </div>
          <div style={ReactDOMRe.Style.make(~gridColumn="3", ~gridRow="2", ())}> right </div>
          <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="3", ())}> bottom </div>
        </div>
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
    <div className="text-xl mt-6">
      {ReasonReact.string({j|Waiting for $n more $friends...|j})}
    </div>
    <div className="flex justify-around">
    <button className="btn btn-grey mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Cancel")}
    </button>
    <button className="btn btn-blue mt-4" onClick=onGoPublicClick>
      {ReasonReact.string("Go Public")}
    </button>
    </div>
  </div>
  </>
};
