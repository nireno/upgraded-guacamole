open AppPrelude;

[@react.component]
let make = (~weTeamId, ~maybeTeamHigh, ~maybeTeamLow, ~maybeTeamJack, ~maybeTeamGame) => {
  let teamIdToName = teamIdtoName(weTeamId);
  <div>
    <div>
      {ReasonReact.string(
         switch (maybeTeamHigh) {
         | None => "No one has high"
         | Some(teamHigh) => teamIdToName(teamHigh) ++ " have high."
         },
       )}
    </div>
    <div>
      {ReasonReact.string(
         switch (maybeTeamLow) {
         | None => "No one has low"
         | Some(teamLow) => teamIdToName(teamLow) ++ " have low."
         },
       )}
    </div>
    <div>
      {switch (maybeTeamJack) {
       | None => ReasonReact.null
       | Some((team, value)) =>
         switch (value) {
         | GameAward.HangJackAward =>
           let hangJackMsg = team == weTeamId ? teamIdToName(weTeamId) ++ " hang the jack."
           : teamIdToName(team) ++ " hang we jack.";
           <div> {ReasonReact.string(hangJackMsg)} </div>
         | RunJackAward =>
           <div> {ReasonReact.string(teamIdToName(team) ++ " get away with jack.")} </div>
         | _ => ReasonReact.null
         }
       }}
    </div>
    <div>
      {switch (maybeTeamGame) {
       | None => ReasonReact.string("Tied for game.")
       | Some(teamGame) => ReasonReact.string(teamIdToName(teamGame) ++ " get game.")
       }}
    </div>
  </div>;
};
