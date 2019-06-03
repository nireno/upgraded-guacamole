open AppPrelude;
[@bs.val] external node_env: string = "process.env.NODE_ENV";

[@bs.val] external username: Js.Nullable.t(string) = "g_display_name";
let username = 
  username 
  |> Js.Nullable.toOption 
  |> Js.Option.getWithDefault("");

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
    let (state, dispatch) = React.useReducer(ClientGame.reducer, ClientGame.initialState);
    let (maybeSocket, setMaybeSocket) = React.useState(() => None);

    let ((southCard, southZ), (eastCard, eastZ), (northCard, northZ), (westCard, westZ)) =
      Player.playersAsQuad(~startFrom=state.me, ())
      |> Quad.map(playerId =>
           GamePlayers.select(
             playerId,
             x => (x.pla_card, Player.turnDistance(state.leader, playerId)),
             state.players,
           )
         );

    /**
    When it is time to remove cards from the board, state.leader should also
    be the trick winner. So this will determine the direction/player the cards
    should animate toward.
    */
    let animationLeaveTo = switch( Player.turnDistance(state.me, state.leader)){
      | 1 => CardTransition.East
      | 2 => CardTransition.North
      | 3 => CardTransition.West
      | _ => CardTransition.South
    };

    React.useEffect1(() => {
      let socket = ClientSocket.T.create();
      setMaybeSocket(_ => Some(socket));
      ClientSocket.T.on(socket, x =>
        switch (x) {
        | SetState(ioClientState) =>
          switch (ClientGame.state_decode(ioClientState |> Js.Json.parseExn)) {
          | Belt.Result.Error(_) => ()
          | Belt.Result.Ok(state) =>
            debugState(state, ~ctx="ClientSocket.T.on SetState", ());
            dispatch(MatchServerState(state));
          };
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

      let (weTeam, demTeam) = switch(teamOfPlayer(state.me)){
        | Team.T1 => (GameTeams.get(T1, state.teams), GameTeams.get(T2, state.teams))
        | Team.T2 => (GameTeams.get(T2, state.teams), GameTeams.get(T1, state.teams))
      };
      
      let bgBoard = state.me == state.activePlayer ? " bg-green-500 " : " bg-orange-500 ";

      <div className="all-fours-game font-sans flex flex-col relative">
      {
        state.gameId == "" 
        ? 
        <MenuView>
          <div
            className="app-name text-white text-5xl"
            style={ReactDOMRe.Style.make(
              ~fontFamily="'Days One', sans-serif",
              ~textShadow="-1px 1px 2px black, 1px -1px 0px #00000080",
              (),
            )}>
            {ReasonReact.string("All Fours")}
          </div>
          <button className="btn btn-blue" onClick={sendIO(IO_JoinGame(username))}>
            {ReasonReact.string("Join Game")}
          </button>
        </MenuView>
        :
        switch(state.gamePhase){
        | FindPlayersPhase(n) => 
          <MenuView>
            <Modal visible=true>
              <FindPlayersView n />
              <button className="btn btn-blue" onClick={sendIO(IO_LeaveGame)}>
                {ReasonReact.string("Cancel")}
              </button>
            </Modal>
          </MenuView>
        | _ => 
          <>
            <div className="trump-card self-center">
              <CardTransition.PlayCard
                maybeCard={state.maybeTrumpCard}
                enterFrom=CardTransition.North
                leaveTo=CardTransition.North 
              />
            </div>
            <ScoreboardView
              weScore=weTeam.team_score wePoints=weTeam.team_points 
              demScore=demTeam.team_score demPoints=demTeam.team_points 
            />

            <div className="the-rest flex-grow flex flex-col">
              <div className={"game-board relative flex-grow flex-shrink-0 flex justify-between items-center " ++ bgBoard} >
                <div
                  className="board-card board-card-west flex-shrink-0"
                  style={ReactDOMRe.Style.make(~zIndex=string_of_int(westZ), ())}>
                  <CardTransition.PlayCard
                    maybeCard=westCard
                    enterFrom=CardTransition.West
                    leaveTo=animationLeaveTo
                  />
                </div>
                <div
                  className="board-card board-card-north self-start flex-shrink-0"
                  style={ReactDOMRe.Style.make(~zIndex=string_of_int(northZ), ())}>
                  <CardTransition.PlayCard
                    maybeCard=northCard
                    enterFrom=CardTransition.North
                    leaveTo=animationLeaveTo
                  />
                </div>
                <div
                  className="board-card board-card-south flex-shrink-0 self-end"
                  style={ReactDOMRe.Style.make(~zIndex=string_of_int(southZ), ())}>
                  <CardTransition.PlayCard
                    maybeCard=southCard
                    enterFrom=CardTransition.South
                    leaveTo=animationLeaveTo
                  />
                </div>
                <div
                  className="board-card board-card-east flex-shrink-0"
                  style={ReactDOMRe.Style.make(~zIndex=string_of_int(eastZ), ())}>
                  <CardTransition.PlayCard
                    maybeCard=eastCard
                    enterFrom=CardTransition.East
                    leaveTo=animationLeaveTo
                  />
                </div>
              </div>

              <WaitingMessage 
                activePlayerName={GamePlayers.get(state.activePlayer, state.players).pla_name} 
                player=state.me 
                activePlayer=state.activePlayer 
                activePlayerPhase=state.activePlayerPhase />

              <Player
                sendDeal={sendIO(SocketMessages.IO_Deal)}
                sendStandUp={sendIO(SocketMessages.IO_Stand)}
                sendBeg={sendIO(IO_Beg)}
                sendGiveOne={sendIO(SocketMessages.IO_GiveOne)}
                sendRunPack={sendIO(IO_RunPack)}
                sendReshuffle={sendIO(IO_DealAgain)}
                playerPhase=state.phase
              />

              <div className="player-hand flex flex-col">
                <div
                  className="player-hand__placeholder-row flex flex-row justify-around">
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                  <img className="hand-card" src="./static/img/card_placeholder.svg" />
                </div>
                <Hand
                  handFacing={state.handFacing}
                  maybeLeadCard={state.maybeLeadCard}
                  maybeTrumpCard={state.maybeTrumpCard}
                  handPhase={
                    Player.maybeIdEqual(state.maybePlayerTurn, state.me)
                      ? Hand.FaceUpHand.HandPlayPhase : Hand.FaceUpHand.HandWaitPhase
                  }
                  sendPlayCard={card =>
                    switch (maybeSocket) {
                    | None => ()
                    | Some(socket) =>
                      ClientSocket.T.emit(
                        socket,
                        SocketMessages.(
                          IO_PlayCard(
                            Player.id_encode(state.me) |> Js.Json.stringify,
                            Card.t_encode(card) |> Js.Json.stringify,
                          )
                        ),
                      )
                    }
                  }
                />
              </div>
            </div>


            {
              switch (state.gamePhase) {
              | RoundSummaryPhase =>
                let {maybeTeamHigh, maybeTeamLow, maybeTeamJack, maybeTeamGame} = state;
                <Modal visible=true>
                  <RoundSummaryView
                    weTeamId=teamOfPlayer(state.me)
                    maybeTeamHigh
                    maybeTeamLow
                    maybeTeamJack
                    maybeTeamGame
                    continueClick={sendIO(IO_NewRound)}
                  />
                </Modal>;
              | GameOverPhase =>
                <Modal visible=true>
                  <GameOverView
                    weScore={weTeam.team_score}
                    demScore={demTeam.team_score}
                    playAgainClick={sendIO(IO_PlayAgain)}
                    leaveClick={sendIO(IO_LeaveGame)}
                  />
                </Modal>
              | FindSubsPhase(n, _) => 
                <Modal visible=true>
                  <FindSubsView n />
                </Modal>
              | _ => ReasonReact.null
              };
            }

            // {createPlayerTricks(state.myTricks)}

            <div className="debug-info" style={ReactDOMRe.Style.make(~position="fixed", ~bottom="0", ())}>
              <div className="text-gray-500 text-xs"> {ReasonReact.string(Player.stringOfId(state.me))} </div>
              <div className="text-gray-500 text-xs"> {ReasonReact.string("GameId: " ++ state.gameId ++ " ")} </div>
            </div>
          </>
        }
      }
      </div>;
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
