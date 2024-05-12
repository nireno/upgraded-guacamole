open Belt

let make: unit => ServerState.db = () => {
  let gameState = MockGameState.make()
  let gameKey = gameState.game_id->Game.game_idToKey

  {
    db_game: Map.String.fromArray([(gameKey, gameState)]) /* game_id -> Game.state */,
    db_game_timer: Map.String.empty /* game_key -> Timer.timeout */,
    db_server_started_at: Js.Date.make(),
    db_public_games_created: 1,
    db_private_games_created: 0,
    db_public_games_started: 0,
    db_private_games_started: 0,
  }
}
