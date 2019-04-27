let component = ReasonReact.statelessComponent("WaitingMessage");

let make = (~player, ~activePlayer, ~activePlayerPhase, _children) => {
  {
    ...component,
    render: (_self) => {
      let str_activePlayer = Player.stringOfId(activePlayer);
      let str_waitingFor = player == activePlayer ? "You " : "Waiting for " ++ str_activePlayer;
      let msg = switch(activePlayerPhase){
        | Player.PlayerTurnPhase(_player) => 
          str_waitingFor ++ " to play"
        | PlayerDealPhase =>
          str_waitingFor ++ " to deal"
        | PlayerBegPhase =>
          str_waitingFor ++ " to beg"
        | PlayerGiveOnePhase =>
          str_waitingFor ++ " to run the pack."
        | PlayerRunPackPhase =>
          str_waitingFor ++ " to run the pack again."
          // This last case should never happen. The active player should never be in the PlayerIdlePhase.
        | PlayerIdlePhase => 
          ""
          // <div>{ ReasonReact.string("Waiting for game to start") }</div>
       };
       <div className="title is-4">{ReasonReact.string(msg)}</div>
      }
    }
  }
