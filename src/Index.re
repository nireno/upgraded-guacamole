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
   module BoardTransitionConf = {
    type item = Card.t;

    [@bs.deriving abstract]
    type props = {
      [@bs.optional] left: string,
      [@bs.optional] opacity: string,
    }
    
    let getKey = Card.stringOfCard
  };

  module BoardTransition = ReactSpring.MakeTransition(BoardTransitionConf);

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(ClientGame.reducer, ClientGame.initialState);
    let (maybeSocket, setMaybeSocket) = React.useState(() => None);

    let transitions =
      BoardTransition.useTransition(
        state.board |> Belt.List.toArray,
        BoardTransition.options(
          ~from=BoardTransitionConf.props(~left="-300px", ~opacity="0", ()),
          ~enter=BoardTransitionConf.props(~left="0", ~opacity="1", ()),
          ~leave=BoardTransitionConf.props(~left="300px", ~opacity="0", ()),
          ~trail=100,
        ),
      );

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

      let (wePoints, demPoints) = switch(teamOfPlayer(state.me)){
      | Team.T1 => (state.team1Points, state.team2Points)
      | Team.T2 => (state.team2Points, state.team1Points)
      };
      

      state.gameId == "" 
      ? 
      <div className="flex flex-row justify-around">
        <button className="btn btn-blue" onClick={sendIO(IO_JoinGame(username))}>
          {ReasonReact.string("Join Game")}
        </button> 
      </div>
      :
      switch(state.gamePhase){
      | FindPlayersPhase(n) => <FindPlayersView n />
      | FindSubsPhase(n, _) => <FindSubsView n />
      | _ => 
      <div>
        {
          switch(state.gamePhase){
          | RoundSummaryPhase => 
            let {maybeTeamHigh, maybeTeamLow, maybeTeamJack, maybeTeamGame} = state;
            <Modal visible=true>
              <RoundSummaryView maybeTeamHigh maybeTeamLow maybeTeamJack maybeTeamGame continueClick={sendIO(IO_NewRound)} />
            </Modal>
          | GameOverPhase => 
            <Modal visible=true>
              <GameOverView
                wePoints
                demPoints
                playAgainClick={sendIO(IO_PlayAgain)}
                leaveClick={sendIO(IO_LeaveGame)}
              />
            </Modal>
          | _ => <Modal visible=false />
          }
        }

        <ScoreboardView wePoints demPoints />

        <div className="game-board section flex flex-row justify-around"> 
          // <h4 className=""> {ReasonReact.string("Board")} </h4>
          <div className="current-trick flex-1 flex flex-row justify-around">
              {
                Array.map(
                  (transition: BoardTransition.transition) => {
                    let card = transition->BoardTransition.itemGet;
                    let props = transition->BoardTransition.propsGet;
                    let key = transition->BoardTransition.keyGet;

                    let springStyle =
                      switch (props->BoardTransitionConf.leftGet) {
                      | None => ReactDOMRe.Style.make(~left="0", ())
                      | Some(left) => ReactDOMRe.Style.make(~left, ())
                      };

                    let springStyle =
                      switch (props->BoardTransitionConf.opacityGet) {
                      | None => springStyle
                      | Some(opacity') =>
                        ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
                      };
                    <ReactSpring.AnimatedDiv key className="board-card" style=springStyle>
                      <Card card />
                    </ReactSpring.AnimatedDiv>;
                  },
                  transitions,
                )
                |> ReasonReact.array
              }
          </div>
          <div className="trump-card flex-none">
            {switch (state.maybeTrumpCard) {
            | None => ReasonReact.null
            | Some(kick) => 
              <> 
                <h4 className="size-3"> {ReasonReact.string("Trump")} </h4> 
                <Card card=kick /> 
              </>;
            }}
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

        <div className="flex flex-col justify-around content-center">
            {
              switch (state.handFacing) {
              | ClientGame.FaceDownHand(n) => <Hand.FaceDownHand nCards=n />
              | ClientGame.FaceUpHand(cards) =>
                <Hand.FaceUpHand
                  maybeLeadCard={state.maybeLeadCard}
                  maybeTrumpCard={state.maybeTrumpCard}
                  handPhase={
                    Player.maybeIdEqual(state.maybePlayerTurn, state.me)
                      ? Hand.FaceUpHand.HandPlayPhase : Hand.FaceUpHand.HandWaitPhase
                  }
                  sendPlayCard={card =>
                    switch(maybeSocket){
                      | None => ()
                      | Some(socket) => 
                          ClientSocket.T.emit(
                          socket,
                          SocketMessages.(IO_PlayCard(Player.id_encode(state.me) |> Js.Json.stringify, Card.t_encode(card) |> Js.Json.stringify)))
                    }
                    
                  }
                  cards
                />
              };
            }

        </div>

        // <div className="flex justify-around">
        //   <div className="round-summary column">
        //     {switch (state.gamePhase) {
        //       | _ => ReasonReact.null
        //       }}
        //   </div>
        // </div>

        // {createPlayerTricks(state.myTricks)}

        <div className="debug-info" style={ReactDOMRe.Style.make(~position="fixed", ~bottom="0", ())}>
          <div className="text-gray-500 text-xs"> {ReasonReact.string(Player.stringOfId(state.me))} </div>
          <div className="text-gray-500 text-xs"> {ReasonReact.string("GameId: " ++ state.gameId ++ " ")} </div>
        </div>
      </div>;
      }
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
