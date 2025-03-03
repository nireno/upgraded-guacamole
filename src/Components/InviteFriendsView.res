@val external baseUrlEnv: Js.Nullable.t<string> = "process.env.allfours_base_url"
let baseUrl = switch baseUrlEnv->Js.Nullable.toOption {
| None => "http://localhost:3000"
| Some(url) => url
}

@react.component
let make = (
  ~me,
  ~onLeaveClick,
  ~inviteCode,
  ~emptySeatCount,
  ~players,
  ~onGoPublicClick,
  ~onRotateGuestsClick=?,
  ~onStartGameClick,
  ~private_game_host,
) => {
  let normalizedInviteCode = Shared.normalizeInviteCode(inviteCode)
  let inviteUrl = `${baseUrl}/?g=${normalizedInviteCode}`
  let rotatedPlayers =
    Player.playersAsQuad(~startFrom=me, ())->(Quad.map(playerId => Quad.get(playerId, players), _))

  let initialCopyText = "Copy invite link"
  let (copyText, updateCopyText) = React.useState(() => initialCopyText)
  let friends = Grammar.byNumber(emptySeatCount, "friend")
  let emptySeatText = string_of_int(emptySeatCount)

  /* To aid in cleaning up remnants of `useEffect` when the component unmounts
   There must be better ways to do this. See My__ReactHooks for an example wrt setInterval */
  let maybeCopySuccessTimeout = ref(None)
  let maybeClipboard = ref(None)

  let onCopyFailure = _event => updateCopyText(_ => "Sorry. Please manually copy the invite code.")

  let rec onCopySuccess = _event =>
    switch maybeClipboard.contents {
    | None => ()
    | Some(clipboard) =>
      updateCopyText(_ =>
        "Link copied! Now paste the link in a text message and send it to your friends."
      )
      clipboard->Clipboard.off("success")
      clipboard->Clipboard.off("error")

      let timeoutId = Js.Global.setTimeout(() => {
        updateCopyText(_ => initialCopyText)
        clipboard->Clipboard.on("success", onCopySuccess)
        clipboard->Clipboard.on("error", onCopyFailure)
      }, 15000)
      maybeCopySuccessTimeout := Some(timeoutId)
    }

  React.useEffect0(() => {
    let stringOfTrigger = _ => inviteUrl
    let clipboard = Clipboard.make(".copy-invite-code", Clipboard.options(~text=stringOfTrigger))
    clipboard->Clipboard.on("success", onCopySuccess)
    clipboard->Clipboard.on("error", onCopyFailure)
    maybeClipboard := Some(clipboard)
    Some(
      () => {
        switch maybeCopySuccessTimeout.contents {
        | None => ()
        | Some(timeoutId) => Js.Global.clearTimeout(timeoutId)
        }
        clipboard->Clipboard.off("success")
        clipboard->Clipboard.off("error")
      },
    )
  })
  <>
    <div className="text-center text-xl"> {React.string(`Invite friends`)} </div>
    {
      let (bottom, right, top, left) =
        rotatedPlayers->(Quad.map(({ClientGame.pla_profile_maybe: pla_profile_maybe}) =>
            switch pla_profile_maybe {
            | None => <EmptySeatAvatarView />
            | Some({client_identicon, client_profile_type}) =>
              <img
                src={LibAvatar.getAvatarUri(~client_id=client_identicon, ~client_profile_type)}
                className="rounded border border-gray-300 w-full "
              />
            }
          , _))

      <div
        className="w-1/2 my-4"
        style={ReactDOM.Style.make(
          ~display="grid",
          ~gridTemplateColumns="repeat(3, 1fr)",
          ~gridGap="10px",
          (),
        )}>
        <div style={ReactDOM.Style.make(~gridColumn="2", ~gridRow="1", ())}> top </div>
        <div style={ReactDOM.Style.make(~gridColumn="1", ~gridRow="2", ())}> left </div>
        {me == private_game_host
          ? switch onRotateGuestsClick {
            | None => React.null
            | Some(onRotateGuestsClick) =>
              <div style={ReactDOM.Style.make(~gridColumn="2", ~gridRow="2", ())}>
                <div
                  style={ReactDOM.Style.make(~opacity=emptySeatCount == 3 ? "0.5" : "1.0", ())}
                  className={"w-full border border-b-4 border-blue-800 bg-blue-500 p-2 rounded-full" ++ (
                    emptySeatCount == 3 ? " cursor-not-allowed" : " cursor-pointer"
                  )}
                  onClick=?{emptySeatCount == 3 ? None : Some(onRotateGuestsClick)}>
                  <Svg_Control_RotateSeating />
                </div>
              </div>
            }
          : React.null}
        <div style={ReactDOM.Style.make(~gridColumn="3", ~gridRow="2", ())}> right </div>
        <div style={ReactDOM.Style.make(~gridColumn="2", ~gridRow="3", ())}> bottom </div>
      </div>
    }
    <div className="text-center w-full">
      <div> {React.string("Your invite link")} </div>
      <pre style={ReactDOM.Style.make(~whiteSpace="pre-wrap", ())}>
        <a
          className="copy-invite-code block text-xs cursor-pointer border border-solid border-gray-300 bg-gray-100">
          {React.string(inviteUrl)}
        </a>
      </pre>
      <a
        className="copy-invite-code block text-base text-blue-700 hover:text-blue-500 underline cursor-pointer my-2">
        {React.string(copyText)}
      </a>
      {
        //Experimental whatsapp link
        // {
        //   let encodedInviteUrl = Js.Global.encodeURI(inviteUrl);
        //   let whatsappText = "Hi! Join my All Fours game at " ++ encodedInviteUrl;
        //   <a
        //     className="block text-base text-blue-700 hover:text-blue-500 underline cursor-pointer"
        //     target="_blank"
        //     rel="noopener noreferrer"
        //     href={"https://wa.me/?text=" ++ whatsappText}>
        //     {React.string("Share link with WhatsApp")}
        //   </a>;
        // }

        //Experimental whatsapp link 2
        // {
        //   ReactDOM.createElementVariadic(
        //     "a",
        //     ~props=
        //       ReactDOM.objToDOMProps({
        //         "target": "_blank",
        //         "data-action": "share/whatsapp/share",
        //         "href": "whatsapp://send?text=The text to share!",
        //       }),
        //     [|
        //       {
        //         React.string(inviteUrl);
        //       },
        //     |],
        //   );
        // }

        // Share invite code
        // <div className="mt-2">
        //   {React.string({j|Or tell your friends to select "Join Private Game" from the "Main Menu" and submit the invite code below.|j})}
        // </div>
        // <div className="mt-2 mb-2"> {React.string({j|$inviteCode|j})} </div>

        emptySeatCount == 0
          ? <div>
              <span> {React.string("Game starting in: ")} </span>
              <CountdownView from=SharedGame.settings.gameStartingCountdownSeconds />
            </div>
          : <div className="text-xl mt-6">
              {React.string(`Waiting for ${emptySeatText} more ${friends}...`)}
            </div>
      }
      <div className="flex justify-around">
        <button className="btn btn-grey mt-4" onClick=onLeaveClick>
          {React.string("Cancel")}
        </button>
        {me == private_game_host
          ? emptySeatCount == 0
              ? <button className="btn btn-blue mt-4" onClick=onStartGameClick>
                  {React.string("Start Now")}
                </button>
              : <button className="btn btn-blue mt-4" onClick=onGoPublicClick>
                  {React.string("Go Public")}
                </button>
          : React.null}
      </div>
    </div>
  </>
}
