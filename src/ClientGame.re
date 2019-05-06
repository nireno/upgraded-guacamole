open AppPrelude;
include SharedGame;

type hand = FaceUpHand(Hand.FaceUpHand.t) | FaceDownHand(Hand.FaceDownHand.t);

[@bs.deriving {jsConverter: newType}]
type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  me: Player.id,
  dealer: Player.id,
  leader: Player.id,
  activePlayer: Player.id,
  activePlayerPhase: Player.phase,
  maybePlayerTurn: option(Player.id),
  hand: hand,
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


let initialState = () => {
  gameId: "initializing",
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3),
  me: P1,
  dealer: P1,
  leader: P1,
  activePlayer: P1,
  activePlayerPhase: PlayerIdlePhase,
  maybePlayerTurn: None,
  hand: FaceDownHand(0),
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

let stringOfState = (state) => {
  "ClientGame.state."
    ++ "{" ++ str_crlf
    ++ str_tab ++ "phase: " ++ Player.stringOfPhase(state.phase) ++ str_crlf
    ++ str_tab ++ "gamePhase: " ++ Game.stringOfPhase(state.gamePhase) ++ str_crlf
    ++ "}" ++ str_crlf
}

let debugState = (state, ~ctx="", ~n=0, ()) => {
  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(state->stringOfState->leftPad(~n=n+1, ()))
}
