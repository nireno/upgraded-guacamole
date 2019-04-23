open AppPrelude;
include SharedGame;

[@bs.deriving {jsConverter: newType}]
type state = {
  phase,
  me: Player.id,
  dealer: Player.id,
  leader: Player.id,
  maybePlayerTurn: option(Player.id),
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
};

type action =
  | StartGame
  | SetState(state)
  | JoinPlayer(int);

let reducer = (action, state) => {
  switch (action) {
  | StartGame => ReasonReact.NoUpdate
  | SetState(state) => ReasonReact.Update(state)
  | JoinPlayer(n) =>
    let {phase} = state;
    switch (phase) {
    | FindPlayersPhase(_n) =>
      let updatePhase = state => {...state, phase: FindPlayersPhase(4 - n)};
      ReasonReact.Update(state |> updatePhase);
    | _ => ReasonReact.NoUpdate
    };
  };
};
