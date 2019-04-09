let div = <div />;
let createElement = ({ReasonReact.state}) => {
  let teamWinner = Game.(state.team1Points >= 14 ? Team.T1 : T2);

  <div>
    <h1> {ReasonReact.string("Game over")} </h1>
    <div>
      {Team.stringOfTeam(teamWinner) ++ " wins!" |> ReasonReact.string}
    </div>
  </div>;
};
