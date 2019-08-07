open AppPrelude;

let dispatch: ServerState.msg => unit;
let dispatchMany: list(ServerState.msg) => unit;
let getState: unit => ServerState.db;
let getGameBySocket: sock_id => option(Game.state);
let getGamesWhere:
  (~phase: Game.Filter.phase=?, ~privacy: Game.Filter.privacy=?, unit) => list(Game.state);
