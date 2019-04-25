[@bs.val] external node_env: string = "process.env.NODE_ENV";
open ClientGame;

let isPlayerTurn = (turn, playerId) => {
  switch (turn) {
  | None => false
  | Some(turn) => turn == playerId
  };
};

let handToPlayerCards: (Player.id, Hand.t) => list((Player.id, Card.t)) =
  (player, hand) => {
    List.map(card => (player, card), hand);
  };


let socket = ClientSocket.T.create();

module App = {
  let component = ReasonReact.reducerComponent("AllFoursApp");

  let make = _children => {
    ...component,
    initialState: ClientGame.initialState,
    reducer: ClientGame.reducer,
    didMount: ({send}) => {
      ClientSocket.T.on(socket, x =>
        switch (x) {
        | SetState(jsonString) =>
          let state = SocketMessages.clientGameStateOfJsonUnsafe(jsonString)
          debugState(state, ());
          send(MatchServerState(state));
        }
      );
    },

    render: self => {
      let {ReasonReact.state, send: _send} = self;
      // let sendActionEvent = (action, _event) => send(action);
      let sendIO= (ioAction, _event) => ClientSocket.T.emit(socket, ioAction)

      let playerPhase =
        Game.playerPhase(state.phase, state.dealer, state.leader, state.maybePlayerTurn, state.me);
      Js.log2("player phase: ", Player.stringOfPhase(playerPhase));
      let _createPlayerTricks = tricks => {
        <div className="column">
          {List.length(tricks) == 0
             ? <div> {ReasonReact.string("No tricks")} </div>
             : <div>
                 {List.map(
                    trick => {
                      <div key={Trick.stringOfTrick(trick)} className="section"> 
                        <Trick trick /> 
                      </div>
                    },
                    tricks,
                  )
                  |> Belt.List.toArray
                  |> ReasonReact.array}
               </div>}
        </div>;
      };

      <div>
        <div>
          {switch (state.maybeTrumpCard) {
           | None => ReasonReact.null
           | Some(kick) => 
             <div> 
               <h1> {ReasonReact.string("Trump")} </h1> 
               <Card card=kick /> 
             </div>;
           }}
        </div>
        <div> {ReasonReact.string(Player.stringOfId(state.me))} </div>
        <Player
          id={state.me}
          sendDeal={sendIO(SocketMessages.IO_Deal)}
          sendStandUp={sendIO(SocketMessages.IO_Stand)}
          sendBeg={sendIO(IO_Beg)}
          sendGiveOne={sendIO(SocketMessages.IO_GiveOne)}
          sendRunPack={sendIO(IO_RunPack)}
          playerPhase
        />
        {
          switch (state.phase) {
          | FindPlayersPhase(n) =>
            let playersAsText = Grammer.byNumber(n, "player");
            let nAsText = string_of_int(n);
            <div> {ReasonReact.string({j|Finding $nAsText more $playersAsText ...|j})} </div>;
          | FindSubsPhase(n, _phase) => 
            let playersAsText = Grammer.byNumber(n, "player");
            let nAsText = string_of_int(n);
            <div> {ReasonReact.string({j|$nAsText $playersAsText disconnected. Finding substitutes...|j})} </div>;
          | _ => ReasonReact.null
          };
        }
        <Hand
          maybeLeadCard={state.maybeLeadCard}
          maybeTrumpCard={state.maybeTrumpCard}
          handPhase={
            Player.maybeIdEqual(state.maybePlayerTurn, state.me)
              ? Hand.HandPlayPhase : Hand.HandWaitPhase
          }
          sendPlayCard = {
            card =>
              ClientSocket.T.emit(
                socket,
                SocketMessages.(IO_PlayCard(ioOfPlayer(state.me), jsonOfCardUnsafe(card))),
              );
          }
          cards={state.hand} />
          <h2> {ReasonReact.string("Board")} </h2>
          {List.length(state.board) == 0
              ? <div> {ReasonReact.string("No cards on the board")} </div>
              : <div />}
          <ul>
            {List.map(
                c =>
                  <Card
                    key={Card.stringOfCard(c)}
                    card=c
                    clickAction=?None
                  />,
                state.board,
              )
              |> Belt.List.toArray
              |> ReasonReact.array}
          </ul>

          <div className="column">
            {switch (state.phase) {
             | GameOverPhase => GameOverPhase.createElement(self)
             | PackDepletedPhase =>
               <div>
                 <div> {ReasonReact.string("No more cards")} </div>
                 <button onClick={sendIO(IO_DealAgain)}>
                   {ReasonReact.string("Reshuffle")}
                 </button>
               </div>
             | RoundSummaryPhase =>
               <div>
                 <div>
                   {ReasonReact.string(
                      switch (state.maybeTeamHigh) {
                      | None => "No one has high"
                      | Some(teamHigh) =>
                        Team.stringOfTeam(teamHigh) ++ " has high."
                      },
                    )}
                 </div>
                 <div>
                   {ReasonReact.string(
                      switch (state.maybeTeamLow) {
                      | None => "No one has low"
                      | Some(teamLow) =>
                        Team.stringOfTeam(teamLow) ++ " has low."
                      },
                    )}
                 </div>
                 <div>
                   {switch (state.maybeTeamJack) {
                    | None => ReasonReact.null
                    | Some((team, value)) =>
                      switch (value) {
                      | HangJackAward =>
                        <div>
                          {ReasonReact.string(
                             Team.stringOfTeam(team) ++ " hanged the jack.",
                           )}
                        </div>
                      | RunJackAward =>
                        <div>
                          {ReasonReact.string(
                             Team.stringOfTeam(team)
                             ++ " gets away with jack.",
                           )}
                        </div>
                      | _ => ReasonReact.null
                      }
                    }}
                 </div>
                 <div>
                   {switch (state.maybeTeamGame) {
                    | None => ReasonReact.string("Tied for game.")
                    | Some(teamGame) =>
                      ReasonReact.string(
                        Team.stringOfTeam(teamGame) ++ " gets game.",
                      )
                    }}
                 </div>
                 <button onClick={sendIO(IO_NewRound)}>
                   {ReasonReact.string("Continue")}
                 </button>
               </div>
             | _ => ReasonReact.null
             }}
          </div>

      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
