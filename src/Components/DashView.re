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
    <pre className="game-state">
      {ReasonReact.string(
         Game.debugOfState(gameState) |> Obj.magic |> Js.Json.stringifyWithSpace(_, 2),
       )}
    </pre>;
  };
};

[@react.component]
let make = (~gameStates) => {
  <div className="dash"> 
  {
    gameStates
    ->Belt.Array.map(gameState => <GameState key={gameState.Game.game_id} gameState />)
    ->ReasonReact.array;
  }
  </div>;
};
