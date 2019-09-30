[@react.component]
let make = (~emptySeatCount, ~canSub, ~onLeaveClick, ~onSubClick, ~players, ~me) => {
  let playersText = Grammar.byNumber(emptySeatCount, "player");
  let emptySeatText = string_of_int(emptySeatCount);
  let rotatedPlayers =  
    Player.playersAsQuad(~startFrom=me, ())
    ->Quad.map(playerId => Quad.get(playerId, players), _);
  
  <>
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
        ? <div className="text-xl mt-6">
            <span> {ReasonReact.string("Game starting in: ")} </span>
            <CountdownView from={SharedGame.settings.gameStartingCountdownSeconds} />
          </div>
        : <div className="text-xl mt-6">
            {ReasonReact.string({j|Finding $emptySeatText more $playersText|j})}
          </div>;
    }
    
  </div>
  {
    let (bottom, right, top, left) =
      rotatedPlayers
      ->Quad.map(
          ({ClientGame.pla_profile_maybe}) =>
            switch (pla_profile_maybe) {
            | None => <EmptySeatAvatarView />
            | Some({client_identicon, client_profile_type}) =>
              let identicon_style =
                ClientSettings.dicebearTypeOfProfileType(client_profile_type);
              <img
                src={j|https://avatars.dicebear.com/v2/$identicon_style/$client_identicon.svg|j}
                className="rounded border border-gray-300 w-full rounded"
              />;
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
      <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="1", ())}> top </div>
      <div style={ReactDOMRe.Style.make(~gridColumn="1", ~gridRow="2", ())}> left </div>
      <div style={ReactDOMRe.Style.make(~gridColumn="3", ~gridRow="2", ())}> right </div>
      <div style={ReactDOMRe.Style.make(~gridColumn="2", ~gridRow="3", ())}> bottom </div>
    </div>;
  }
  <button className="btn btn-blue mt-4" onClick=onLeaveClick>
    {ReasonReact.string("Cancel")}
  </button>
  </>
};
