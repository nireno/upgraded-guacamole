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
          debugState(state, ~ctx="ClientSocket.T.on SetState", ());
          send(MatchServerState(state));
        }
      );
    },

    render: self => {
      let {ReasonReact.state, send: _send} = self;
      // let sendActionEvent = (action, _event) => send(action);
      let sendIO= (ioAction, _event) => ClientSocket.T.emit(socket, ioAction)

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

      <div className="">
        <div className="scoreboard flex-column">
          <h3 className="text-center"> {ReasonReact.string("Scoreboard")} </h3>
          <h5 className="text-center"> {ReasonReact.string("Team 1: " ++ string_of_int(state.team1Points) ++ " ")} </h5>
          <h5 className="text-center"> {ReasonReact.string("Team 2: " ++ string_of_int(state.team2Points) ++ " ")} </h5>
        </div>
        <WaitingMessage player=state.me activePlayer=state.activePlayer activePlayerPhase=state.activePlayerPhase  />
        <Player
          id={state.me}
          sendDeal={sendIO(SocketMessages.IO_Deal)}
          sendStandUp={sendIO(SocketMessages.IO_Stand)}
          sendBeg={sendIO(IO_Beg)}
          sendGiveOne={sendIO(SocketMessages.IO_GiveOne)}
          sendRunPack={sendIO(IO_RunPack)}
          playerPhase=state.phase
        />
        {
          let msg = switch (state.gamePhase) {
          | FindPlayersPhase(n) =>
            let playersAsText = Grammar.byNumber(n, "player");
            let nAsText = string_of_int(n);
            {j|Finding $nAsText more $playersAsText ...|j}
          | FindSubsPhase(n, _phase) => 
            let playersAsText = Grammar.byNumber(n, "player");
            let nAsText = string_of_int(n);
            {j|$nAsText $playersAsText disconnected. Finding substitutes...|j}
          | _ => 
            ""
          };
          msg == "" ? ReasonReact.null : <div className="text-center text-white bg-orange my-5 p-2"> {ReasonReact.string({msg})} </div>;

        }
        <div className="flex justify-around content-center">
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
          <div className="game-board section"> 

            <div className="trump-card">
              {switch (state.maybeTrumpCard) {
              | None => ReasonReact.null
              | Some(kick) => 
                <> 
                  <h4 className="size-3"> {ReasonReact.string("Trump")} </h4> 
                  <Card card=kick /> 
                </>;
              }}
            </div>

            <h4 className=""> {ReasonReact.string("Board")} </h4>
            <div className="current-trick">
              {List.length(state.board) == 0
                  ? <div> {ReasonReact.string("No cards on the board")} </div>
                  : <div />}
              <div>
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
              </div>
            </div>
          </div>
        </div>

        <div className="flex justify-around">
          <div className="round-summary column">
            {switch (state.gamePhase) {
              | GameOverPhase => GameOverPhase.createElement(self)
              | PackDepletedPhase =>
                <div>
                  <div> {ReasonReact.string("No more cards")} </div>
                  <button className="btn btn-blue" onClick={sendIO(IO_DealAgain)}>
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
                  <button className="btn btn-blue" onClick={sendIO(IO_NewRound)}>
                    {ReasonReact.string("Continue")}
                  </button>
                </div>
              | _ => ReasonReact.null
              }}
          </div>
        </div>

        <div className="text-orange text-xs"> {ReasonReact.string(Player.stringOfId(state.me))} </div>
        <div className="text-orange text-xs"> {ReasonReact.string("GameId: " ++ state.gameId ++ " ")} </div>
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
