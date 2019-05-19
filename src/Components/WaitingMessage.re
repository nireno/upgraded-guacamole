[@react.component]
let make = (~player, ~activePlayer, ~activePlayerName, ~activePlayerPhase) => {
  let str_waitingFor = player == activePlayer ? "You " : "Waiting for " ++ activePlayerName;
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
        "text-center text-white p-2 my-4 " ++ (player == activePlayer ? "bg-green-500" : "bg-orange-500")
      }>
      {ReasonReact.string(msg)}
    </div>;
};
