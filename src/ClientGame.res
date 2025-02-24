include SharedGame

@spice type maybeCard = option<Card.t>

@spice type maybeTeamHigh = option<GameAward.luckyAwardData>
@spice type maybeTeamLow = option<GameAward.luckyAwardData>
@spice type maybeTeamJack = option<GameAward.jackAwardData>
@spice type maybeTeamGame = option<GameAward.gameAwardData>

@spice
type clientProfile = {
  client_username: string,
  client_identicon: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
}

@spice
type playerState = {
  pla_card: option<Card.t>,
  pla_profile_maybe: option<clientProfile>, //optional since a user might disconnect from the game
}

let initPlayerState = () => {pla_card: None, pla_profile_maybe: None}

@spice
type partnerInfo = {
  trumpCount: int,
  cardsToDisplay: list<Card.t>,
}

@spice
type findSubsContext = {emptySeatCount: int}

@spice
type findPlayersContext = {
  emptySeatCount: int,
  canSub: bool /* when there exists a public game in FindSubsPhase */,
}

@spice
type begPhaseContext = BegPhaseDeciding | BegPhaseStanding

@spice
type phase =
  | IdlePhase(idleReason)
  | FindSubsPhase(findSubsContext)
  | FindPlayersPhase(findPlayersContext)
  | DealPhase
  | BegPhase(begPhaseContext)
  | GiveOnePhase
  | RunPackPhase
  | FlipFinalTrumpPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase(Quad.t<rematchDecision>)

@spice
type state = {
  gameId: game_id,
  phase: Player.phase,
  gamePhase: phase,
  players: (playerState, playerState, playerState, playerState),
  me: Player.id,
  maybePartnerInfo: option<partnerInfo>,
  myTricks: list<Trick.t>,
  teams: (teamState, teamState),
  dealer: Player.id,
  leader: Player.id,
  handFacing: Hand.handFacing,
  maybeLeadCard: maybeCard,
  maybeTrumpCard: maybeCard,
}

let stringOfPhase = x =>
  switch x {
  | IdlePhase(_reason) => "IdlePhase"
  | FindSubsPhase(_) => "FindSubsPhase"
  | FindPlayersPhase(_) => "FindPlayersPhase"
  | DealPhase => "DealPhase"
  | BegPhase(BegPhaseDeciding) => "BegPhase(BegPhaseDeciding)"
  | BegPhase(BegPhaseStanding) => "BegPhase(BegPhaseStanding)"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | FlipFinalTrumpPhase => "FlipFinalTrumpPhase"
  | PlayerTurnPhase(_) => "PlayerTurnPhase"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase(_) => "GameOverPhase"
  }
