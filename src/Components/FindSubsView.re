[@react.component]
let make = (~emptySeatCount as n, ~onLeaveClick, ~players: Quad.t(ClientGame.playerState), ~me) => {
  let playersWord = Grammar.byNumber(n, "player");
  let nText = string_of_int(n);
  let rotatedPlayers =  
    Player.playersAsQuad(~startFrom=me, ())
    ->Quad.map(playerId => Quad.get(playerId, players), _);
  <>
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
    {
      n == 0
        ? <div className="mt-6 text-xl text-center">
            <span> {ReasonReact.string("Game resuming in: ")} </span>
            <CountdownView from={SharedGame.settings.gameStartingCountdownSeconds} />
          </div>
        : 
        <div className="text-center">
          <div> {ReasonReact.string({j|$nText $playersWord disconnected|j})} </div>
          <div> {ReasonReact.string({j|Please wait while I find substitutes...|j})} </div>
        </div>
    }
    <button className="btn btn-blue mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Leave Game")}
    </button>
  </>
};
