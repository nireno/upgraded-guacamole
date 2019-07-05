open AppPrelude;
include SharedGame;

[@decco] type maybePlayerId = option(Player.id);
[@decco] type maybeTeamId = option(Team.id);
[@decco] type maybeTeamJackAward = option( (Team.id, GameAward.award) );
[@decco] type maybeCard = option(Card.t);

[@decco]
type playerState = {
  pla_name: string,
  pla_card: option(Card.t),
};

[@decco]
type partnerInfo = {
  trumpCount: int,
  cardsToDisplay: list(Card.t),
};

[@decco]
type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  players: (playerState, playerState, playerState, playerState),
  me: Player.id,
  maybePartnerInfo: option(partnerInfo),
  myTricks: list(Trick.t),
  teams: (teamState, teamState),
  dealer: Player.id,
  leader: Player.id,
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
    {pla_name: Player.stringOfId(N1), pla_card: None},
    {pla_name: Player.stringOfId(N2), pla_card: None},
    {pla_name: Player.stringOfId(N3), pla_card: None},
    {pla_name: Player.stringOfId(N4), pla_card: None},
  ),
  me: N1,
  maybePartnerInfo: None,
  myTricks: [],
  teams: (initialTeamState, initialTeamState),
  dealer: N1,
  leader: N1,
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

let reducer = (prevState, action) => {
  switch (action) {
  | MatchServerState(nextState) => 

    // Prevent user from navigating away from an in-progress game.
    if(prevState.gameId == "" && nextState.gameId != ""){
      Raw.addUnloadListener(Raw.preventUnloadListener);
    } else if(prevState.gameId != "" && nextState.gameId == "") {
      Raw.removeUnloadListener(Raw.preventUnloadListener);
    } else {
      ()
    };

    nextState
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
