// This helps with delivering notifications
// I only want to deliver to `Connected` clients
let mapNotiToSocketMaybe = (gameState, noti) =>
  switch gameState.Game.clients->(Quad.get(noti.Noti.noti_recipient, _)) {
  | Attached(client) => Some({ServerEvent.sock_id: client.client_socket_id, toast: noti})
  | _ => None
  }

let clientGamePhaseOfGamePhase = x =>
  switch x {
  | Game.Phase.IdlePhase(idleReason) => ClientGame.IdlePhase(idleReason)
  | FindSubsPhase({emptySeatCount}) => FindSubsPhase({emptySeatCount: emptySeatCount})
  | FindPlayersPhase({emptySeatCount, canSub}) => FindPlayersPhase({emptySeatCount, canSub})
  | DealPhase => DealPhase
  | BegPhase(BegPhaseDeciding) => BegPhase(BegPhaseDeciding)
  | BegPhase(BegPhaseStanding) => BegPhase(BegPhaseStanding)
  | GiveOnePhase => GiveOnePhase
  | RunPackPhase => RunPackPhase
  | FlipFinalTrumpPhase => FlipFinalTrumpPhase
  | PlayerTurnPhase(seatId) => PlayerTurnPhase(seatId)
  | PackDepletedPhase => PackDepletedPhase
  | GameOverPhase(rematchDecisionQuad) => GameOverPhase(rematchDecisionQuad)
  }
