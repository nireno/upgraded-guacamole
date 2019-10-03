open AppPrelude;

let dispatch: ServerEvent.event => unit;
let dispatchMany: list(ServerEvent.event) => unit;
let getState: unit => ServerState.db;
let getGameBySocket: sock_id => option(Game.state);
// let getGamesWhere:
//   (~phase: Game.Filter.simplePhase=?, ~privacy: Game.Filter.privacy=?, unit) => list(Game.state);
