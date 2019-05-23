open AppPrelude;
include SharedGame;

[@decco] type handFacing = | FaceUpHand(Hand.FaceUpHand.t) | FaceDownHand(Hand.FaceDownHand.t);
[@decco] type maybePlayerId = option(Player.id);
[@decco] type maybeTeamId = option(Team.id);
[@decco] type maybeTeamJackAward = option( (Team.id, award) );
[@decco] type maybeCard = option(Card.t);

[@decco]
type playerState = {
  pla_name: string
};

[@decco]
type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  players: (playerState, playerState, playerState, playerState),
  me: Player.id,
  myTricks: list(Trick.t),
  dealer: Player.id,
  leader: Player.id,
  activePlayer: Player.id,
  activePlayerPhase: Player.phase,
  maybePlayerTurn: maybePlayerId,
  handFacing: handFacing,
  maybeLeadCard: maybeCard,
  maybeTrumpCard: maybeCard,
  board: list(Card.t),
  team1Points: int,
  team2Points: int,
  // team1GamePoints: int,
  // team2GamePoints: int,
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
    {pla_name: Player.stringOfId(P1)},
    {pla_name: Player.stringOfId(P2)},
    {pla_name: Player.stringOfId(P3)},
    {pla_name: Player.stringOfId(P4)},
  ),
  me: P1,
  myTricks: [],
  dealer: P1,
  leader: P1,
  activePlayer: P1,
  activePlayerPhase: PlayerIdlePhase,
  maybePlayerTurn: None,
  handFacing: FaceDownHand(0),
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
