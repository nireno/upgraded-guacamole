type t = {
  id: Player.id,
  phase: Player.phase
};

let find: (Game.phase, Player.id) => option(t) = (gamePhase, dealerId) => {
  switch(gamePhase){
  | DealPhase => Some({id: dealerId, phase: Player.PlayerDealPhase})
  | RunPackPhase => Some({id: dealerId, phase: Player.PlayerRunPackPhase})
  | PackDepletedPhase => Some({id: dealerId, phase: Player.PlayerRedealPhase})

  | BegPhase => Some({id: Player.nextPlayer(dealerId), phase: Player.PlayerBegPhase})
  | GiveOnePhase => Some({id: dealerId, phase: Player.PlayerGiveOnePhase})

  | PlayerTurnPhase(playerId) => Some({id: playerId, phase: Player.PlayerTurnPhase(playerId)})

  | FindPlayersPhase(_)
  | FindSubsPhase(_, _) 
  | IdlePhase
  | GameOverPhase => None
  }
};
