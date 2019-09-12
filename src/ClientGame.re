include SharedGame;

[@decco] type maybeCard = option(Card.t);

[@decco] type maybeTeamHigh = option(GameAward.luckyAwardData);
[@decco] type maybeTeamLow = option(GameAward.luckyAwardData);
[@decco] type maybeTeamJack = option(GameAward.jackAwardData);
[@decco] type maybeTeamGame = option(GameAward.gameAwardData);

[@decco]
type clientProfile = {
  client_username: string,
  client_identicon: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
};

[@decco]
type playerState = {
  pla_card: option(Card.t),
  pla_profile_maybe: option(clientProfile) //optional since a user might disconnect from the game
};

let initPlayerState = () => {pla_card: None, pla_profile_maybe: None};

[@decco]
type partnerInfo = {
  trumpCount: int,
  cardsToDisplay: list(Card.t),
};

[@decco]
type findSubsContext = {
  emptySeatCount: int
};

[@decco]
type findPlayersContext = {
  emptySeatCount: int,
  canSub: bool /* when there exists a public game in FindSubsPhase */
};

[@decco]
type phase =
  | IdlePhase
  | FindSubsPhase(findSubsContext)
  | FindPlayersPhase(findPlayersContext)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase(Quad.t(rematchDecision));

[@decco]
type state = {
  gameId: game_id,
  phase: Player.phase,
  gamePhase: phase,
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
};

let initialState = {
  gameId: Public(""),
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase({ emptySeatCount: 3, canSub: false }),
  players: Quad.make(_ => initPlayerState()),
  me: N1,
  maybePartnerInfo: None,
  myTricks: [],
  teams: (initialTeamState, initialTeamState),
  dealer: N1,
  leader: N1,
  handFacing: FaceDownHand(0),
  maybeLeadCard: None,
  maybeTrumpCard: None,
};

