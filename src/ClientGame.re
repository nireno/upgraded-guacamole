open AppPrelude;
include SharedGame;

[@bs.deriving {jsConverter: newType}]
type state = {
  phase,
  me: Player.id,
  dealer: Player.id,
  leader: Player.id,
  maybePlayerTurn: option(Player.id),
  hand: list(Card.t),
  maybeLeadCard: option(Card.t),
  maybeTrumpCard: option(Card.t),
  board: list(Card.t),
  team1Points: int,
  team2Points: int,
  maybeTeamHigh: option(Team.id),
  maybeTeamLow: option(Team.id),
  maybeTeamJack: option((Team.id, award)),
  maybeTeamGame: option(Team.id),
};


/**
  For parsing a stringified version of state back to one I can work with in
  Reason. Used when passing data via socket.io.
 */
[@bs.scope "JSON"] [@bs.val]
external stateOfJson: string => abs_state = "parse";
let stateOfJson = json => json |> stateOfJson |> stateFromJs;

let debugState: (state, ~depth: int=?, unit) => unit =
  (state, ~depth=0, ()) => {
    debuggin("Client Game State:", ~depth, ());
    let depth = depth + 1;
    debuggin("me: " ++ Player.stringOfId(state.me), ~depth, ());
    SharedGame.debugPhase(state.phase, ~depth, ());
    debuggin("maybeDealer: " ++ Player.stringOfId(state.dealer), ~depth, ());
    debuggin("maybeLeader: " ++ Player.stringOfId(state.leader), ~depth, ());
    debuggin("maybePlayerTurn: " ++ Player.stringOfMaybeId(state.maybePlayerTurn), ~depth, ());
  };

let initialState = () => {
  phase: FindPlayersPhase(3),
  me: P1,
  dealer: P1,
  leader: P1,
  maybePlayerTurn: None,
  hand: [],
  maybeLeadCard: None,
  maybeTrumpCard: None,
  board: [],
  team1Points: 0,
  team2Points: 0,
  maybeTeamHigh: None,
  maybeTeamLow: None,
  maybeTeamJack: None,
  maybeTeamGame: None,
};

type action =
  | MatchServerState(state)

let reducer = (action, _state) => {
  switch (action) {
  | MatchServerState(state) => ReasonReact.Update(state)
  };
};
