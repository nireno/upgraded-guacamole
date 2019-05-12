let createElement = (team1Points, team2Points) => {
  let teamWinner = ClientGame.(team1Points >= team2Points ? Team.T1 : T2);

  <div>
    <h1> {ReasonReact.string("Game over")} </h1>
    <div>
      {Team.stringOfTeam(teamWinner) ++ " wins!" |> ReasonReact.string}
    </div>
  </div>;
};
