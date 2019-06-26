module PlayerState = {
  [@react.component]
  let make = (~playerId, ~playerState) => {
    <div className="player_state">
      <div> {playerId |> Player.stringOfId |> ReasonReact.string} </div>
      <div>
      {
        switch(playerState.Game.pla_socket){
          | None => "not connected"
          | Some(_) => playerState.pla_name ++ " connected"
        }
        |> ReasonReact.string
      }
      </div>
    </div>;
  };
};

module GameState = {
  [@react.component]
  let make = (~gameState) => {
    <div className="game-state">
      <h6> {ReasonReact.string(gameState.Game.roomKey)} </h6>
      <div> {gameState.phase |> Game.stringOfPhase |> ReasonReact.string} </div>
      <div className="player-states">
      {
        gameState.players
         |> GamePlayers.toDict
         |> List.map(((playerId, playerState)) => <PlayerState key=Player.stringOfId(playerId) playerId playerState />)
         |> Belt.List.toArray
         |> ReasonReact.array
      }
      </div>
    </div>;
  };
};

[@react.component]
let make = (~children) => {
  <div className="dash"> children </div>;
};
