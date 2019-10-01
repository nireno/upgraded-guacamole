// This helps with delivering notifications
// I only want to deliver to `Connected` clients
let mapNotiToSocketMaybe = (gameState, noti) => {
  switch (gameState.Game.clients->Quad.get(noti.Noti.noti_recipient, _)) {
  | Connected(client) => Some({ServerEvent.sock_id: client.client_socket_id, toast: noti})
  | _ => None
  };
};

let clientGamePhaseOfGamePhase =
  fun
  | Game.IdlePhase(idleReason) => ClientGame.IdlePhase(idleReason)
  | FindSubsPhase({emptySeatCount}) =>
    ClientGame.(FindSubsPhase({emptySeatCount: emptySeatCount}))
  | FindPlayersPhase({emptySeatCount, canSub}) => FindPlayersPhase({emptySeatCount, canSub})
  | DealPhase => DealPhase
  | BegPhase => BegPhase
  | GiveOnePhase => GiveOnePhase
  | RunPackPhase => RunPackPhase
  | PlayerTurnPhase(seatId) => PlayerTurnPhase(seatId)
  | PackDepletedPhase => PackDepletedPhase
  | GameOverPhase(rematchDecisionQuad) => GameOverPhase(rematchDecisionQuad)
  ;