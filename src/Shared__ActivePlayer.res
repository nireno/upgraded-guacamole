type t = {
  id: Player.id,
  phase: Player.phase,
}

@ocaml.doc(" phase and dealerId is sufficient for deducing if there is an active player ")
let find: (ClientGame.phase, Player.id) => option<t> = (gamePhase, dealerId) =>
  switch gamePhase {
  | DealPhase => Some({id: dealerId, phase: Player.PlayerDealPhase})
  | RunPackPhase => Some({id: dealerId, phase: Player.PlayerRunPackPhase})
  | FlipFinalTrumpPhase => Some({id: dealerId, phase: Player.PlayerFlipFinalTrumpPhase})
  | PackDepletedPhase => Some({id: dealerId, phase: Player.PlayerRedealPhase})

  | BegPhase(BegPhaseDeciding) =>
    Some({id: Quad.nextId(dealerId), phase: Player.PlayerBegPhase(PlayerBegPhaseDeciding)})
  | BegPhase(BegPhaseStanding) =>
    Some({id: Quad.nextId(dealerId), phase: Player.PlayerBegPhase(PlayerBegPhaseStanding)})
  | GiveOnePhase => Some({id: dealerId, phase: Player.PlayerGiveOnePhase})

  | PlayerTurnPhase(playerId) => Some({id: playerId, phase: Player.PlayerTurnPhase(playerId)})

  | FindPlayersPhase(_)
  | FindSubsPhase(_)
  | IdlePhase(_)
  | GameOverPhase(_) =>
    None
  }
