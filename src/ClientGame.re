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
  | IdlePhase(idleReason)
  | FindSubsPhase(findSubsContext)
  | FindPlayersPhase(findPlayersContext)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | FlipFinalTrumpPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase(Quad.t(rematchDecision))
  ;

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


let stringOfPhase = fun
  | IdlePhase(_reason) => "IdlePhase"
  | FindSubsPhase(_) => "FindSubsPhase"
  | FindPlayersPhase(_) => "FindPlayersPhase"
  | DealPhase => "DealPhase"
  | BegPhase => "BegPhase"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | FlipFinalTrumpPhase => "FlipFinalTrumpPhase"
  | PlayerTurnPhase(_) => "PlayerTurnPhase"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase(_) => "GameOverPhase"
  ;