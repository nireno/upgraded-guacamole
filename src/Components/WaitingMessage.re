let component = ReasonReact.statelessComponent("WaitingMessage");

let make = (~player, ~activePlayer, ~activePlayerPhase, _children) => {
  {
    ...component,
    render: (_self) => {
      let str_activePlayer = Player.stringOfId(activePlayer);
      let str_waitingFor = player == activePlayer ? "You " : "Waiting for " ++ str_activePlayer;
      switch(activePlayerPhase){
        | Player.PlayerTurnPhase(_player) => 
          <div>{ ReasonReact.string(str_waitingFor ++ " to play") }</div>
        | PlayerDealPhase =>
          <div>{ ReasonReact.string(str_waitingFor ++ " to deal") }</div>
        | PlayerBegPhase =>
          <div>{ ReasonReact.string(str_waitingFor ++ " to beg") }</div>
        | PlayerGiveOnePhase =>
          <div>{ ReasonReact.string(str_waitingFor ++ " to run the pack.") }</div>
        | PlayerRunPackPhase =>
          <div>{ ReasonReact.string(str_waitingFor ++ " to run the pack again.") }</div>
          // This last case should never happen. The active player should never be in the PlayerIdlePhase.
        | PlayerIdlePhase => 
          ReasonReact.null
          // <div>{ ReasonReact.string("Waiting for game to start") }</div>
       }
      }
    }
  }
