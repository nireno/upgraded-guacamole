@react.component
let make = (
  ~myPlayerId,
  ~maybeActivePlayer: option<Shared.ActivePlayer.t>,
  ~activePlayerName,
  (),
) => {
  let (amIActive, activePlayerPhase: Player.phase) = switch maybeActivePlayer {
  | None => (false, PlayerIdlePhase)
  | Some(activePlayer) =>
    activePlayer.id == myPlayerId ? (true, activePlayer.phase) : (false, activePlayer.phase)
  }

  let str_waitingFor = amIActive ? "You " : "Waiting for " ++ activePlayerName

  let msg = switch activePlayerPhase {
  | Player.PlayerTurnPhase(_player) => str_waitingFor ++ " to play"
  | PlayerDealPhase => str_waitingFor ++ " to deal"
  | PlayerBegPhase(PlayerBegPhaseDeciding) => str_waitingFor ++ " to beg"
  | PlayerBegPhase(PlayerBegPhaseStanding) => str_waitingFor ++ " to play"
  | PlayerGiveOnePhase => str_waitingFor ++ " to run the pack."
  | PlayerRunPackPhase => str_waitingFor ++ " to run the pack again."
  | PlayerFlipFinalTrumpPhase => str_waitingFor ++ " to kick again."
  | PlayerRedealPhase => str_waitingFor ++ " to redeal."
  | PlayerIdlePhase => "..."
  }

  <div
    className={"waiting-message text-center text-white text-lg p-2 shadow-sm rounded-b " ++ (
      amIActive ? "bg-green-600 text-shadow--play-msg" : "bg-orange-600 text-shadow--wait-msg"
    )}>
    {React.string(msg)}
  </div>
}
