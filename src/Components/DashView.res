module PlayerState = {
  @react.component
  let make = (~playerId, ~playerState) =>
    <div className="player_state">
      <div> {React.string(Player.stringOfId(playerId))} </div>
      <div>
        {React.string(
          switch playerState.ClientGame.pla_profile_maybe {
          | None => "not connected"
          | Some(profile) => profile.client_username ++ " connected"
          },
        )}
      </div>
    </div>
}

module GameState = {
  @react.component
  let make = (~gameState) =>
    <pre className="game-state">
      {React.string((Js.Json.stringifyWithSpace(_, 2))(Obj.magic(Game.debugOfState(gameState))))}
    </pre>
}

@react.component
let make = (~gameStates) =>
  <div className="dash">
    {gameStates
    ->Belt.Array.map(gameState =>
      <GameState key={Game.stringOfGameId(gameState.Game.game_id)} gameState />
    )
    ->React.array}
  </div>
