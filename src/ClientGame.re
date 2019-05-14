open AppPrelude;
include SharedGame;

type hand = FaceUpHand(Hand.FaceUpHand.t) | FaceDownHand(Hand.FaceDownHand.t);

[@bs.deriving {jsConverter: newType}]
type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  p1Name: string,
  p2Name: string,
  p3Name: string,
  p4Name: string,
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


let initialState = {
  gameId: "",
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3),
  p1Name: Player.stringOfId(P1),
  p2Name: Player.stringOfId(P2),
  p3Name: Player.stringOfId(P3),
  p4Name: Player.stringOfId(P4),
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

let reducer = (_state, action) => {
  switch (action) {
  | MatchServerState(state) => state
  };
};

let getPlayerName = (playerId, state) => {
  switch(playerId){
    | Player.P1 => state.p1Name
    | Player.P2 => state.p2Name
    | Player.P3 => state.p3Name
    | Player.P4 => state.p4Name
  }
}

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
