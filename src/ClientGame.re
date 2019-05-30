open AppPrelude;
include SharedGame;

[@decco] type maybePlayerId = option(Player.id);
[@decco] type maybeTeamId = option(Team.id);
[@decco] type maybeTeamJackAward = option( (Team.id, award) );
[@decco] type maybeCard = option(Card.t);

[@decco]
type playerState = {
  pla_name: string,
  pla_card: option(Card.t),
};

[@decco]
type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  players: (playerState, playerState, playerState, playerState),
  me: Player.id,
  myTricks: list(Trick.t),
  teams: (teamState, teamState),
  dealer: Player.id,
  leader: Player.id,
  activePlayer: Player.id,
  activePlayerPhase: Player.phase,
  maybePlayerTurn: maybePlayerId,
  handFacing: Hand.handFacing,
  maybeLeadCard: maybeCard,
  maybeTrumpCard: maybeCard,
  maybeTeamHigh: maybeTeamId,
  maybeTeamLow: maybeTeamId,
  maybeTeamJack: maybeTeamJackAward,
  maybeTeamGame: maybeTeamId,
};

let initialState = {
  gameId: "",
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3),
  players: (
    {pla_name: Player.stringOfId(P1), pla_card: None},
    {pla_name: Player.stringOfId(P2), pla_card: None},
    {pla_name: Player.stringOfId(P3), pla_card: None},
    {pla_name: Player.stringOfId(P4), pla_card: None},
  ),
  me: P1,
  myTricks: [],
  teams: (initialTeamState, initialTeamState),
  dealer: P1,
  leader: P1,
  activePlayer: P1,
  activePlayerPhase: PlayerIdlePhase,
  maybePlayerTurn: None,
  handFacing: FaceDownHand(0),
  maybeLeadCard: None,
  maybeTrumpCard: None,
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


let stringOfState = (state) => {
  "ClientGame.state."
    ++ "{" ++ str_crlf
    ++ str_tab ++ "phase: " ++ Player.stringOfPhase(state.phase) ++ str_crlf
    ++ str_tab ++ "gamePhase: " ++ SharedGame.stringOfPhase(state.gamePhase) ++ str_crlf
    ++ "}" ++ str_crlf
}

let debugState = (state, ~ctx="", ~n=0, ()) => {
  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(state->stringOfState->leftPad(~n=n+1, ()))
}
