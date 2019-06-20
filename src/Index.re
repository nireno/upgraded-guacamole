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
    let (notis, setNotis) = React.useState(() => []);
    let (appRect, setAppRect) =
      React.useState(() => Webapi.Dom.DomRect.make(~x=0.0, ~y=0.0, ~width=0.0, ~height=0.0));

    let appRef: React.Ref.t(Js.Nullable.t(Webapi.Dom.Element.t)) = React.useRef(Js.Nullable.null);
    React.useEffect0(() => {
      let el = My.Nullable.getUnsafe(React.Ref.current(appRef))
      setAppRect(_ => Webapi.Dom.Element.getBoundingClientRect(el))
      None
    });

    let {ActivePlayer.id: activePlayerId, phase: activePlayerPhase} = 
      switch(ActivePlayer.find(state.gamePhase, state.dealer)){
      | None => {ActivePlayer.id: P1, phase: PlayerIdlePhase}
      | Some(activePlayer) => activePlayer
      };

    let ((southCard, southZ, southTags), (eastCard, eastZ, eastTags), (northCard, northZ, northTags), (westCard, westZ, westTags)) =
      Player.playersAsQuad(~startFrom=state.me, ())
      |> Quad.map(playerId =>
           GamePlayers.select(
             playerId,
             x => {
               let tags = [];
               let tags = state.dealer == playerId ? tags @ [PlayerTagsView.Dealer] : tags;
               let tags = 
                 playerId == activePlayerId ? tags @ [PlayerTagsView.Turner] : tags;
               (x.pla_card, Player.turnDistance(state.leader, playerId), tags);
             },
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
          | Belt.Result.Error(err) => Js.log(err)
          | Belt.Result.Ok(state) =>
            debugState(state, ~ctx="ClientSocket.T.on SetState", ());
            dispatch(MatchServerState(state));
          };
        | AddNotis(ioNotis) => 
          switch (ClientGame.notis_decode(ioNotis |> Js.Json.parseExn)) {
          | Belt.Result.Error(err) => Js.log(err)
          | Belt.Result.Ok(newNotis) =>
            setNotis(notis => notis @ newNotis);
          };
        | Reset => 
          dispatch(MatchServerState(ClientGame.initialState));
          setNotis(_ => []);
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
      
      let bgBoard = state.me == activePlayerId ? " bg-green-500 " : " bg-orange-500 ";
      let handleAppClick = _ => {
        /* Clear notifications when user taps anywhere in the app. */
        setNotis(_ => [])
      };

      <div  
        ref={ReactDOMRe.Ref.domRef(appRef)} 
        className="all-fours-game font-sans flex flex-col relative"
        onClick=handleAppClick>

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
                {
                  Belt.List.forEach(notis, notiToRemove =>
                    switch (notiToRemove.noti_kind) {
                    | Duration(millis) =>
                      Js.Global.setTimeout(() => setNotis(List.filter(noti => noti != notiToRemove)), millis)
                      |> ignore
                    | _ => ()
                    }
                  );
                  <NotificationsView id="notifications_view" notis appRect teamId={teamOfPlayer(state.me)} />;
                }
                <div className="game-board__player">
                  <PlayerTagsView className="player-tags player-tags__west flex flex-col justify-center h-full" tags=westTags />
                  <div
                    className="board-card board-card-west flex-shrink-0"
                    style={ReactDOMRe.Style.make(~zIndex=string_of_int(westZ), ())}>
                    <img className="card__placeholder relative" src="./static/img/card_transparent.svg" />
                    <CardTransition.PlayCard
                      maybeCard=westCard
                      enterFrom=CardTransition.West
                      leaveTo=animationLeaveTo
                    />
                  </div>
                </div>
                <div className="game-board__player game-board__player-north">
                  <div className="game-board__player-offset">
                    <PlayerTagsView className="player-tags player-tags__north flex flex-row justify-center" tags=northTags />
                    <div
                      className="board-card board-card-north self-start flex-shrink-0 mx-auto"
                      style={ReactDOMRe.Style.make(~zIndex=string_of_int(northZ), ())}>
                        <img className="card__placeholder block relative" src="./static/img/card_transparent.svg" />
                        <CardTransition.PlayCard
                          maybeCard=northCard
                          enterFrom=CardTransition.North
                          leaveTo=animationLeaveTo
                        />
                    </div>
                  </div>
                </div>
                <div className="game-board__player game-board__player-south">
                  <div className="game-board__player-offset">
                    <div
                      className="board-card board-card-south flex-shrink-0 self-end mx-auto"
                      style={ReactDOMRe.Style.make(~zIndex=string_of_int(southZ), ())}>
                      <img className="card__placeholder block relative" src="./static/img/card_transparent.svg" />
                      <CardTransition.PlayCard
                        maybeCard=southCard
                        enterFrom=CardTransition.South
                        leaveTo=animationLeaveTo
                      />
                    </div>
                    <PlayerTagsView
                      className="player-tags player-tags__south flex flex-row justify-center"
                      tags=southTags
                    />
                  </div>
                </div>
                <div className="game-board__player game-board__player-east">
                  <div
                    className="board-card board-card-east flex-shrink-0"
                    style={ReactDOMRe.Style.make(~zIndex=string_of_int(eastZ), ())}>
                    <img className="card relative" src="./static/img/card_transparent.svg" />
                    <CardTransition.PlayCard
                      maybeCard=eastCard
                      enterFrom=CardTransition.East
                      leaveTo=animationLeaveTo
                    />
                  </div>
                  <PlayerTagsView className="player-tags player-tags__east flex flex-col justify-center h-full" tags=eastTags />
                </div>
              </div>

              <WaitingMessage 
                activePlayerName={GamePlayers.get(activePlayerId, state.players).pla_name} 
                player=state.me 
                activePlayer=activePlayerId 
                activePlayerPhase=activePlayerPhase />

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
                    activePlayerId == state.me
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
              | FindPlayersPhase(n) =>
                <Modal visible=true>
                  <FindPlayersView n />
                  <button className="btn btn-blue" onClick={sendIO(IO_LeaveGame)}>
                    {ReasonReact.string("Cancel")}
                  </button>
                </Modal>
              | FindSubsPhase(n, _) => 
                <Modal visible=true> 
                  <FindSubsView n /> 
                </Modal>
              | GameOverPhase =>
                <Modal visible=true>
                  <GameOverView
                    weScore={weTeam.team_score}
                    demScore={demTeam.team_score}
                    playAgainClick={sendIO(IO_PlayAgain)}
                    leaveClick={sendIO(IO_LeaveGame)}
                  />
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
      </div>;
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
