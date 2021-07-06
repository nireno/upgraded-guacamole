@ocaml.doc(" Game.phase and dealerId is sufficient for deducing if there is an active player ")
let find: (Game.phase, Player.id) => option<Shared.ActivePlayer.t> = (gamePhase, dealerId) =>
  switch gamePhase {
  | DealPhase => Some({id: dealerId, phase: Player.PlayerDealPhase})
  | RunPackPhase => Some({id: dealerId, phase: Player.PlayerRunPackPhase})
  | FlipFinalTrumpPhase => Some({id: dealerId, phase: Player.PlayerFlipFinalTrumpPhase})
  | PackDepletedPhase => Some({id: dealerId, phase: Player.PlayerRedealPhase})

  | BegPhase => Some({id: Quad.nextId(dealerId), phase: Player.PlayerBegPhase})
  | GiveOnePhase => Some({id: dealerId, phase: Player.PlayerGiveOnePhase})

  | PlayerTurnPhase(playerId) => Some({id: playerId, phase: Player.PlayerTurnPhase(playerId)})

  | FindPlayersPhase(_)
  | FindSubsPhase(_)
  | IdlePhase(_)
  | GameOverPhase(_) =>
    None
  }
