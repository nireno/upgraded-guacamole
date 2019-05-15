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
        | SetState(jsonString) =>
          let state = SocketMessages.clientGameStateOfJsonUnsafe(jsonString)
          debugState(state, ~ctx="ClientSocket.T.on SetState", ());
          dispatch(MatchServerState(state));
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

      let createPlayerTricks = tricks => {
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

      state.gameId == "" 
      ? 
      <div className="flex flex-row justify-around">
        <button className="btn btn-blue" onClick={sendIO(IO_JoinGame(username))}>
          {ReasonReact.string("Join Game")}
        </button> 
      </div>
      :
      <div>
        <div className="scoreboard flex flex-row justify-around">
          <div className="text-center">
            <div> {ReasonReact.string("Us")} </div>
            <div> {ReasonReact.string(string_of_int(state.team1Points))} </div>
          </div>
          <div className="text-center">
            <div> {ReasonReact.string("Them")} </div>
            <div> {ReasonReact.string(string_of_int(state.team2Points))} </div>
          </div>
        </div>


        <div className="game-board section flex flex-row justify-around"> 
          // <h4 className=""> {ReasonReact.string("Board")} </h4>
          <div className="current-trick flex-1 flex flex-row justify-around  m-4">
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
                    <Card key style=springStyle card />
                  },
                  transitions,
                )
                |> ReasonReact.array
              }
          </div>
          <div className="trump-card flex-none m-4">
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
          activePlayerName={ClientGame.getPlayerName(state.activePlayer, state)} 
          player=state.me 
          activePlayer=state.activePlayer 
          activePlayerPhase=state.activePlayerPhase />

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


        <div className="flex flex-row justify-around content-center">
            {
              switch (state.hand) {
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
                          SocketMessages.(IO_PlayCard(ioOfPlayer(state.me), jsonOfCardUnsafe(card))))
                    }
                    
                  }
                  cards
                />
              };
            }

        </div>

        <div className="flex justify-around">
          <div className="round-summary column">
            {switch (state.gamePhase) {
              | GameOverPhase => GameOverPhase.createElement(state.team1Points, state.team2Points)
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
        {createPlayerTricks(state.myTricks)}
        <div className="text-orange text-xs"> {ReasonReact.string(Player.stringOfId(state.me))} </div>
        <div className="text-orange text-xs"> {ReasonReact.string("GameId: " ++ state.gameId ++ " ")} </div>
      </div>;
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
