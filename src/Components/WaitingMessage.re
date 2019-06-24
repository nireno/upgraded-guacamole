[@react.component]
let make = (~myPlayerId, ~maybeActivePlayer: option(ActivePlayer.t), ~activePlayerName, ()) => {
  let (amIActive, activePlayerPhase: Player.phase) =
    switch (maybeActivePlayer) {
    | None => (false, PlayerIdlePhase)
    | Some(activePlayer) =>
      activePlayer.id == myPlayerId
        ? (true, activePlayer.phase) : (false, activePlayer.phase)
    };

  let str_waitingFor = amIActive ? "You " : "Waiting for " ++ activePlayerName;

  let msg =
      switch (activePlayerPhase) {
      | Player.PlayerTurnPhase(_player) => str_waitingFor ++ " to play"
      | PlayerDealPhase => str_waitingFor ++ " to deal"
      | PlayerBegPhase => str_waitingFor ++ " to beg"
      | PlayerGiveOnePhase => str_waitingFor ++ " to run the pack."
      | PlayerRunPackPhase => str_waitingFor ++ " to run the pack again."
      | PlayerRedealPhase => str_waitingFor ++ " to redeal."
      | PlayerIdlePhase => "..."
      };

  <div
    className={
      "waiting-message text-center text-white text-lg p-2 shadow-sm mb-2 rounded-b "
      ++ (amIActive 
            ? "bg-green-600 text-shadow--play-msg" 
            : "bg-orange-600 text-shadow--wait-msg")
    }>
    {ReasonReact.string(msg)}
  </div>;
};
