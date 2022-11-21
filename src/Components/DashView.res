module PlayerState = {
  @react.component
  let make = (~playerId, ~playerState) =>
    <div className="player_state">
      <div> {playerId |> Player.stringOfId |> React.string} </div>
      <div>
        {switch playerState.ClientGame.pla_profile_maybe {
        | None => "not connected"
        | Some(profile) => profile.client_username ++ " connected"
        } |> React.string}
      </div>
    </div>
}

module GameState = {
  @react.component
  let make = (~gameState) =>
    <pre className="game-state">
      {React.string(
        Game.debugOfState(gameState) |> Obj.magic |> Js.Json.stringifyWithSpace(_, 2),
      )}
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
