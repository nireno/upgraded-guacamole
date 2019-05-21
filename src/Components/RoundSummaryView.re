[@react.component]
let make = (~maybeTeamHigh, ~maybeTeamLow, ~maybeTeamJack, ~maybeTeamGame, ~continueClick) => {
  <>
    <div>
      {ReasonReact.string(
         switch (maybeTeamHigh) {
         | None => "No one has high"
         | Some(teamHigh) => Team.stringOfTeam(teamHigh) ++ " has high."
         },
       )}
    </div>
    <div>
      {ReasonReact.string(
         switch (maybeTeamLow) {
         | None => "No one has low"
         | Some(teamLow) => Team.stringOfTeam(teamLow) ++ " has low."
         },
       )}
    </div>
    <div>
      {switch (maybeTeamJack) {
       | None => ReasonReact.null
       | Some((team, value)) =>
         switch (value) {
         | SharedGame.HangJackAward =>
           <div> {ReasonReact.string(Team.stringOfTeam(team) ++ " hanged the jack.")} </div>
         | RunJackAward =>
           <div> {ReasonReact.string(Team.stringOfTeam(team) ++ " gets away with jack.")} </div>
         | _ => ReasonReact.null
         }
       }}
    </div>
    <div>
      {switch (maybeTeamGame) {
       | None => ReasonReact.string("Tied for game.")
       | Some(teamGame) => ReasonReact.string(Team.stringOfTeam(teamGame) ++ " gets game.")
       }}
    </div>
    <button className="mt-4 btn btn-blue" onClick=continueClick>
      {ReasonReact.string("Continue")}
    </button>
  </>;
};
