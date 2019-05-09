[@bs.val] external node_env: string = "process.env.NODE_ENV";
open ClientGame;

let isPlayerTurn = (turn, playerId) => {
  switch (turn) {
  | None => false
  | Some(turn) => turn == playerId
  };
};

module App = {
  [@react.component]
  let make = () => {
    let (gameId, setGameId) = React.useState(() => "initializing");
    let (phase, setPhase) = React.useState(() => Player.PlayerIdlePhase);
    let (gamePhase, setGamePhase) = React.useState(() => FindPlayersPhase(3));
    let (me, setMe) = React.useState(() => Player.P1);
    let (dealer, setDealer) = React.useState(() => Player.P1);
    let (leader, setLeader) = React.useState(() => Player.P1);
    let (activePlayer, setActivePlayer) = React.useState(() => Player.P1);
    let (activePlayerPhase, setActivePlayerPhase) = React.useState(() => Player.PlayerIdlePhase);
    let (maybePlayerTurn, setMaybePlayerTurn) = React.useState(() => None);
    let (hand, setHand) = React.useState(() => FaceDownHand(0));
    let (maybeLeadCard, setMaybeLeadCard) = React.useState(() =>None);
    let (maybeTrumpCard, setMaybeTrumpCard) = React.useState(() =>None);
    let (board, setBoard) = React.useState(() => []);
    let (team1Points, setTeam1Points) = React.useState(() => 0);
    let (team2Points, setTeam2Points) = React.useState(() => 0);
    let (maybeTeamHigh, setMaybeTeamHigh) = React.useState(() => None);
    let (maybeTeamLow, setMaybeTeamLow) = React.useState(() => None);
    let (maybeTeamJack, setMaybeTeamJack) = React.useState(() => None);
    let (maybeTeamGame, setMaybeTeamGame) = React.useState(() => None);
    let (maybeSocket, setMaybeSocket) = React.useState(() => None);

    React.useEffect1(() => {
      let socket = ClientSocket.T.create();
      setMaybeSocket(_ => Some(socket));
      ClientSocket.T.on(socket, x =>
        switch (x) {
        | SetState(jsonString) =>
          let state = SocketMessages.clientGameStateOfJsonUnsafe(jsonString)
          debugState(state, ~ctx="ClientSocket.T.on SetState", ());
          setGameId(_ => state.gameId);
          setPhase(_ => state.phase);
          setGamePhase(_ => state.gamePhase);
          setMe(_ => state.me);
          setDealer(_ => state.dealer);
          setLeader(_ => state.leader);
          setActivePlayer(_ => state.activePlayer);
          setActivePlayerPhase(_ => state.activePlayerPhase);
          setMaybePlayerTurn(_ => state.maybePlayerTurn);
          setHand(_ => state.hand);
          setMaybeLeadCard(_ => state.maybeLeadCard);
          setMaybeTrumpCard(_ => state.maybeTrumpCard);
          setBoard(_ => state.board);
          setTeam1Points(_ => state.team1Points);
          setTeam2Points(_ => state.team2Points);
          setMaybeTeamHigh(_ => state.maybeTeamHigh);
          setMaybeTeamLow(_ => state.maybeTeamLow);
          setMaybeTeamJack(_ => state.maybeTeamJack);
          setMaybeTeamGame(_ => state.maybeTeamGame);
        }
      );
      None
    }, [||])

      let sendIO = (ioAction, _event) => {
        switch (maybeSocket) {
        | None => ()
        | Some(socket) => ClientSocket.T.emit(socket, ioAction)
        };
      };

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
          <h5 className="text-center"> {ReasonReact.string("Team 1: " ++ string_of_int(team1Points) ++ " ")} </h5>
          <h5 className="text-center"> {ReasonReact.string("Team 2: " ++ string_of_int(team2Points) ++ " ")} </h5>
        </div>
        <WaitingMessage player=me activePlayer=activePlayer activePlayerPhase=activePlayerPhase  />
        <Player
          id={me}
          sendDeal={sendIO(SocketMessages.IO_Deal)}
          sendStandUp={sendIO(SocketMessages.IO_Stand)}
          sendBeg={sendIO(IO_Beg)}
          sendGiveOne={sendIO(SocketMessages.IO_GiveOne)}
          sendRunPack={sendIO(IO_RunPack)}
          playerPhase=phase
        />
        {
          let msg = switch (gamePhase) {
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
          <div>
            <h4> {ReasonReact.string("Your hand")} </h4>
            {
              switch (hand) {
              | ClientGame.FaceDownHand(n) => <Hand.FaceDownHand nCards=n />
              | ClientGame.FaceUpHand(cards) =>
                <Hand.FaceUpHand
                  maybeLeadCard={maybeLeadCard}
                  maybeTrumpCard={maybeTrumpCard}
                  handPhase={
                    Player.maybeIdEqual(maybePlayerTurn, me)
                      ? Hand.FaceUpHand.HandPlayPhase : Hand.FaceUpHand.HandWaitPhase
                  }
                  sendPlayCard={card =>
                    switch(maybeSocket){
                      | None => ()
                      | Some(socket) => 
                          ClientSocket.T.emit(
                          socket,
                          SocketMessages.(IO_PlayCard(ioOfPlayer(me), jsonOfCardUnsafe(card))))
                    }
                    
                  }
                  cards
                />
              };
            }
          </div>
          <div className="game-board section"> 

            <div className="trump-card">
              {switch (maybeTrumpCard) {
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
              {List.length(board) == 0
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
                    board,
                  )
                  |> Belt.List.toArray
                  |> ReasonReact.array}
              </div>
            </div>
          </div>
        </div>

        <div className="flex justify-around">
          <div className="round-summary column">
            {switch (gamePhase) {
              | GameOverPhase => GameOverPhase.createElement(team1Points, team2Points)
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
                      switch (maybeTeamHigh) {
                      | None => "No one has high"
                      | Some(teamHigh) =>
                        Team.stringOfTeam(teamHigh) ++ " has high."
                      },
                    )}
                  </div>
                  <div>
                    {ReasonReact.string(
                      switch (maybeTeamLow) {
                      | None => "No one has low"
                      | Some(teamLow) =>
                        Team.stringOfTeam(teamLow) ++ " has low."
                      },
                    )}
                  </div>
                  <div>
                    {switch (maybeTeamJack) {
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
                    {switch (maybeTeamGame) {
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

        <div className="text-orange text-xs"> {ReasonReact.string(Player.stringOfId(me))} </div>
        <div className="text-orange text-xs"> {ReasonReact.string("GameId: " ++ gameId ++ " ")} </div>
      </div>;
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
