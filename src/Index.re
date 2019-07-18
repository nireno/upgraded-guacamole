[@bs.val] external node_env: Js.Nullable.t(string) = "process.env.NODE_ENV";
[@bs.val] external allfours_rules_url: Js.Nullable.t(string) = "process.env.allfours_rules_url";
[@bs.val] external allfours_help_url: Js.Nullable.t(string) = "process.env.allfours_help_url";

let node_env = node_env |> Js.Nullable.toOption |> Js.Option.getWithDefault("production");

let isProduction = node_env == "production";

[@bs.val] external username: Js.Nullable.t(string) = "g_display_name";
let username = username |> Js.Nullable.toOption |> Js.Option.getWithDefault("");

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
    let url = ReasonReactRouter.useUrl();
    let (state, dispatch) = React.useReducer(ClientGame.reducer, ClientGame.initialState);
    let (maybeSocket, setMaybeSocket) = React.useState(() => None);
    let (notis, updateNotis) = React.useReducer(Noti.State.reducer, Noti.State.initial);
    let (clientSettings, updateClientSettings) = React.useState(() => LocalStorage.getClientSettings());

    let saveClientSettings = newClientSettings => {
      LocalStorage.updateClientSettings(newClientSettings);
      updateClientSettings(_ => newClientSettings);
    };

    let (appRect, setAppRect) =
      React.useState(() => Webapi.Dom.DomRect.make(~x=0.0, ~y=0.0, ~width=0.0, ~height=0.0));

    let appRef: React.Ref.t(Js.Nullable.t(Webapi.Dom.Element.t)) =
      React.useRef(Js.Nullable.null);

    React.useEffect0(() => {
      let el = My.Nullable.getUnsafe(React.Ref.current(appRef));
      setAppRect(_ => Webapi.Dom.Element.getBoundingClientRect(el));
      None;
    });

    let maybeActivePlayer = ActivePlayer.find(state.gamePhase, state.dealer);
    let activePlayerName =
      maybeActivePlayer
      ->Belt.Option.map(activePlayer => Quad.get(activePlayer.id, state.players).pla_name)
      ->Belt.Option.getWithDefault("");

    let (
      (_southName, southCard, southZ, southTags),
      (eastName, eastCard, eastZ, eastTags),
      (northName, northCard, northZ, northTags),
      (westName, westCard, westZ, westTags),
    ) =
      Player.playersAsQuad(~startFrom=state.me, ())
      |> Quad.map(playerId =>
           Quad.select(
             playerId,
             x => {
               let tags = [];
               let tags = state.dealer == playerId ? tags @ [PlayerTagsView.Dealer] : tags;
               let tags =
                 switch (maybeActivePlayer) {
                 | None => tags
                 | Some({id: activePlayerId}) =>
                   playerId == activePlayerId ? tags @ [PlayerTagsView.Turner] : tags
                 };

               (x.pla_name, x.pla_card, Player.turnDistance(state.leader, playerId), tags);
             },
             state.players,
           )
         );

    /**
    When it is time to remove cards from the board, state.leader should also
    be the trick winner. So this will determine the direction/player the cards
    should animate toward.
    */
    let animationLeaveTo =
      switch (Player.turnDistance(state.me, state.leader)) {
      | 1 => CardTransition.East
      | 2 => CardTransition.North
      | 3 => CardTransition.West
      | _ => CardTransition.South
      };

    React.useEffect0(
      () => {
        let socket = ClientSocket.T.create();
        setMaybeSocket(_ => Some(socket));
        ClientSocket.T.on(socket, x =>
          switch (x) {
          | SetState(ioClientState) =>
            switch (ClientGame.state_decode(ioClientState |> Js.Json.parseExn)) {
            | Belt.Result.Error(err) => Js.log(err)
            | Belt.Result.Ok(state) =>
              dispatch(MatchServerState(state));
            }
          | AddNotis(ioNotis) =>
            switch (ClientGame.notis_decode(ioNotis |> Js.Json.parseExn)) {
            | Belt.Result.Error(err) => Js.log(err)
            | Belt.Result.Ok(newNotis) => updateNotis(Add(newNotis))
            }
          | Reset =>
            dispatch(MatchServerState(ClientGame.initialState));
            updateNotis(Reset(Noti.State.initial));
          | AckOk | AckError(_) => ()
          }
        );
        None;
      }
    );

    let sendIO = (ioAction) => {
      switch (maybeSocket) {
      | None => ()
      | Some(socket) => ClientSocket.T.emit(socket, ioAction)
      };
    };

    let sendIOWithAck = (ioAction, ack) => {
      switch (maybeSocket) {
      | None => ()
      | Some(socket) => ClientSocket.T.emitWithAck(socket, ioAction, ack)
      };
    };

    let _createPlayerTricks = tricks => {
      <div className="column">
        {List.length(tricks) == 0
           ? <div> {ReasonReact.string("No tricks")} </div>
           : <div>
               {List.map(
                  trick =>
                    <div key={Trick.stringOfTrick(trick)} className="section">
                      <Trick trick />
                    </div>,
                  tricks,
                )
                |> Belt.List.toArray
                |> ReasonReact.array}
             </div>}
      </div>;
    };

    let (weTeam, demTeam) =
      switch (teamOfPlayer(state.me)) {
      | Team.T1 => (GameTeams.get(T1, state.teams), GameTeams.get(T2, state.teams))
      | Team.T2 => (GameTeams.get(T2, state.teams), GameTeams.get(T1, state.teams))
      };

    let bgBoard =
      switch (maybeActivePlayer) {
      | None => " bg-orange-500 "
      | Some({id: activePlayerId}) =>
        state.me == activePlayerId ? " bg-green-500 " : " bg-orange-500 "
      };

    let handleAppClick = _ => {
      /* Clear notifications when user taps anywhere in the app. */
      updateNotis( RemoveKind(Noti.Confirm) );
    };

    let addUniqueTimoutNoti = msg => {
      let isTimeoutNoti = noti =>
        switch (noti.Noti.noti_kind) {
        | Duration(_) => true
        | _ => false
        };

      let similarMessages =
        List.filter(noti => isTimeoutNoti(noti) && noti.noti_message == Text(msg), notis);
      switch (similarMessages) {
      | [] =>
        updateNotis(
          AddOne(
            Noti.{
              noti_id: Nanoid.nanoid(),
              noti_recipient: state.me,
              noti_level: Danger,
              noti_kind: Duration(5000),
              noti_message: Text(msg),
            },
          ),
        )
      | _ => ()
      };
    };

    let handleCreatePrivateGameClick = _event => {
      sendIO(
        IO_StartPrivateGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
      );
    };

    let sendIoJoinPrivateGame = (inviteCode, ack) => {
      sendIOWithAck(
        IO_JoinPrivateGame(inviteCode, username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
        ack,
      );
    };
    
    let stringOfGameId = SharedGame.stringOfGameId(state.gameId);
    /** 
      Reading the url backwords allows the app to work when it isn't served from
      the root url a website i.e. it will work whether the app is served from the
      root url `http://localhost/` or some sub-path like `allfours` in `http://example.com/allfours`
    */
    switch (List.rev(url.path)) {
    | ["feedback", ..._rest] => 
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col justify-center relative mx-auto">
        <MenuView>
          <ExperimentalView
            onJoinClick={_event => {
              ReasonReactRouter.push("./");
              sendIO(
                IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
              );
            }}
          />
        </MenuView>
      </div>;
    | ["settings", ..._rest] => 
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col justify-center relative mx-auto">
        <MenuView>
          <SettingsView onSave=saveClientSettings settings=clientSettings />
        </MenuView>
      </div>;
    | ["private-games", ..._rest] => 
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col justify-center relative mx-auto">
        <MenuView> <JoinPrivateGameView sendJoinGame=sendIoJoinPrivateGame /> </MenuView>
      </div>;
    | _ => 
    <div
      ref={ReactDOMRe.Ref.domRef(appRef)}
      className="all-fours-game font-sans flex flex-col relative mx-auto"
      onClick=handleAppClick>
      {stringOfGameId == ""
         ? <MenuView>
             <div
               className="app-name text-white text-5xl"
               style={ReactDOMRe.Style.make(
                 ~fontFamily="'Days One', sans-serif",
                 ~textShadow="-1px 1px 2px black, 1px -1px 0px #00000080",
                 (),
               )}>
               {ReasonReact.string("All Fours")}
             </div>
             {
               let onClick = _event =>
                 node_env == "production"
                   ? ReasonReactRouter.push("./feedback")
                   : sendIO(
                       IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
                     );
               <button className="btn btn-blue mt-1" onClick>
                 {ReasonReact.string("Join Public Game")}
               </button>
             }
             <button className="btn btn-blue mt-1" onClick={_ => ReasonReactRouter.push("./private-games/")}>
               {ReasonReact.string("Join Private Game")}
             </button>
             <button className="btn btn-blue mt-1" onClick=handleCreatePrivateGameClick>
               {ReasonReact.string("Create Private Game")}
             </button>
             <div className="link link-white mt-4" 
                  onClick={_ => ReasonReactRouter.push("./settings")}>
               {ReasonReact.string("Settings")}
             </div>
             {switch (Js.Nullable.toOption(allfours_rules_url)) {
              | None => ReasonReact.null
              | Some(allfours_rules_url) =>
                <div
                  className="help absolute w-full flex justify-around"
                  style={ReactDOMRe.Style.make(~bottom="5%", ())}>
                  <div className="w-1/2 flex flex-col items-center p-4">
                    <a className="w-full text-blue-700 hover:text-blue-500 flex-grow text-center" href=allfours_rules_url>
                      <div className="w-full text-center">
                        <svg
                          className="fill-current w-4"
                          xmlns="http://www.w3.org/2000/svg"
                          viewBox="0 0 20 20">
                          <path d=Icon.question />
                        </svg>
                      </div>
                      <span>{ReasonReact.string("All Fours Rules ")}</span>
                    </a>
                  </div>
                  {switch (Js.Nullable.toOption(allfours_help_url)) {
                   | None => <div className="w-1/2"></div>
                   | Some(allfours_tutorial_url) =>
                     <div className="w-1/2 flex flex-col items-center p-4">
                       <a className="w-full text-blue-700 hover:text-blue-500 flex-grow text-center" href=allfours_tutorial_url>
                         <div className="w-full text-center">
                           <svg
                             className="fill-current w-4"
                             xmlns="http://www.w3.org/2000/svg"
                             viewBox="0 0 20 20">
                             <path d=Icon.question />
                           </svg>
                         </div>
                         <span>{ReasonReact.string("How to play")}</span>
                       </a>
                     </div>
                  }}
                </div>
              }}
           </MenuView>
         : <>
             <div className="trump-card self-center">
               <CardTransition.PlayCard
                 maybeCard={state.maybeTrumpCard}
                 enterFrom=CardTransition.North
                 leaveTo=CardTransition.North
               />
             </div>
             <ScoreboardView
               weScore={weTeam.team_score}
               wePoints={weTeam.team_points}
               demScore={demTeam.team_score}
               demPoints={demTeam.team_points}
             />
             <div className="the-rest flex-grow flex flex-col">
               <div
                 className={
                   "game-board relative flex-grow flex-shrink-0 flex justify-between items-center "
                   ++ bgBoard
                 }>
                 {
                   Belt.List.forEach(notis, notiToRemove =>
                     switch (notiToRemove.noti_kind) {
                     | Duration(millis) =>
                       Js.Global.setTimeout(() => updateNotis(Remove(notiToRemove)), millis)
                       |> ignore
                     | _ => ()
                     }
                   );
                   <NotificationsView
                     id="notifications_view"
                     notis
                     appRect
                     teamId={teamOfPlayer(state.me)}
                   />;
                 }
                 <div className="game-board__player">
                   <PlayerTagsView
                     className="player-tags player-tags__west flex flex-col justify-center h-full"
                     tags=westTags
                   />
                   <div
                     className="board-card board-card-west flex-shrink-0"
                     style={ReactDOMRe.Style.make(~zIndex=string_of_int(westZ), ())}>
                     <div className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                       style=ReactDOMRe.Style.make(~transform="translateY(1.5em)", ())>
                       <div className="text-sm h-5">{ReasonReact.string(westName)}</div>
                     </div>
                     <img
                       className="card__placeholder relative block"
                       src="./static/img/card_transparent.svg"
                     />
                     <CardTransition.PlayCard
                       maybeCard=westCard
                       enterFrom=CardTransition.West
                       leaveTo=animationLeaveTo
                     />
                   </div>
                 </div>
                 <div className="game-board__player game-board__player-north">
                   <div className="game-board__player-offset">
                     {
                       switch (state.maybeTrumpCard) {
                       | None => ReasonReact.null
                       | Some(trumpCard) =>
                         switch (state.maybePartnerInfo) {
                         | None => ReasonReact.null
                         | Some(partnerInfo) =>
                           <PlayerTrumpsView
                             suit={trumpCard.suit}
                             n={partnerInfo.trumpCount}
                             className="w-full absolute"
                             style={ReactDOMRe.Style.make(~transform="translateX(-100%)", ())}
                           />
                         }
                       };
                     }
                     <PlayerTagsView
                       className="player-tags player-tags__north flex flex-row justify-center"
                       tags=northTags
                     />
                     <div
                       className="board-card board-card-north relative self-start flex-shrink-0"
                       style={ReactDOMRe.Style.make(~zIndex=string_of_int(northZ), ())}>
                       <div 
                         className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                         style=ReactDOMRe.Style.make(~transform="translate(-50%, 1.5em)", ())>
                         <div className="text-sm h-5">{ReasonReact.string(northName)}</div>
                       </div>
                       <img
                         className="card__placeholder block relative"
                         src="./static/img/card_transparent.svg"
                       />
                       <CardTransition.PlayCard
                         maybeCard=northCard
                         enterFrom=CardTransition.North
                         leaveTo=animationLeaveTo
                       />
                     </div>
                   </div>
                   {
                     switch (state.maybePartnerInfo) {
                     | None => ReasonReact.null
                     | Some(partnerInfo) =>
                       <PlayerCardTagsView
                         className="absolute w-full top-0 left-0"
                         style={ReactDOMRe.Style.make(~transform="translate(50%)", ())}
                         cards={partnerInfo.cardsToDisplay}
                       />
                     };
                   }
                 </div>
                 <div className="game-board__player game-board__player-south">
                   <div className="game-board__player-offset">
                     <div
                       className="board-card board-card-south flex-shrink-0 self-end mx-auto"
                       style={ReactDOMRe.Style.make(~zIndex=string_of_int(southZ), ())}>
                      //  <div className="absolute w-full h-full overflow-hidden flex flex-col justify-start items-center"
                      //    style=ReactDOMRe.Style.make(~transform="translateY(-1.5em)", ())>
                      //    <div>{ReasonReact.string(southName)}</div>
                      //  </div>
                       <img
                         className="card__placeholder block relative"
                         src="./static/img/card_transparent.svg"
                       />
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
                     <div className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                       style=ReactDOMRe.Style.make(~transform="translateY(1.5em)", ())>
                       <div className="text-sm h-5">{ReasonReact.string(eastName)}</div>
                     </div>
                     <img className="card relative" src="./static/img/card_transparent.svg" />
                     <CardTransition.PlayCard
                       maybeCard=eastCard
                       enterFrom=CardTransition.East
                       leaveTo=animationLeaveTo
                     />
                   </div>
                   <PlayerTagsView
                     className="player-tags player-tags__east flex flex-col justify-center h-full"
                     tags=eastTags
                   />
                 </div>
               </div>
               <WaitingMessage activePlayerName myPlayerId={state.me} maybeActivePlayer />
               <Player
                 sendDeal={_event => sendIO(SocketMessages.IO_Deal)}
                 sendStandUp={_event => sendIO(SocketMessages.IO_Stand)}
                 sendBeg={_event => sendIO(IO_Beg)}
                 sendGiveOne={_event => sendIO(SocketMessages.IO_GiveOne)}
                 sendRunPack={_event => sendIO(IO_RunPack)}
                 sendReshuffle={_event => sendIO(IO_DealAgain)}
                 playerPhase={state.phase}
               />
               <div className="player-hand flex flex-col">
                 <div className="player-hand__placeholder-row flex flex-row justify-around">
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
                     switch (maybeActivePlayer) {
                     | None => Hand.FaceUpHand.HandWaitPhase
                     | Some(activePlayer) =>
                       activePlayer.id == state.me
                         ? Hand.FaceUpHand.HandPlayPhase : Hand.FaceUpHand.HandWaitPhase
                     }
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
                   onInvalidCardClick=addUniqueTimoutNoti
                 />
               </div>
             </div>
             {switch (state.gamePhase) {
              | FindPlayersPhase(n, canSub) =>
                <Modal visible=true>
                  {
                    switch (state.gameId) {
                    | Public(_) =>
                      <FindPlayersView
                        n
                        canSub
                        onLeaveClick={_event => sendIO(IO_LeaveGame)}
                        onSubClick={_event => sendIO(IO_Substitute(username))}
                      />
                    | Private(str_game_id) => <InviteFriendsView n inviteCode=str_game_id />
                    };
                  }
                </Modal>
              | FindSubsPhase(n, _) => <Modal visible=true> <FindSubsView n /> </Modal>
              | GameOverPhase =>
                <Modal visible=true>
                  <GameOverView
                    weScore={weTeam.team_score}
                    demScore={demTeam.team_score}
                    playAgainClick={_event => sendIO(IO_PlayAgain(username, clientSettings |> ClientSettings.t_encode |> Js.Json.stringify))}
                    leaveClick={_event => sendIO(IO_LeaveGame)}
                  />
                </Modal>
              | _ => ReasonReact.null
              }}
             // {createPlayerTricks(state.myTricks)}
             {if (isProduction) {
                ReasonReact.null;
              } else {
                <div
                  className="debug-info"
                  style={ReactDOMRe.Style.make(~position="fixed", ~bottom="0", ())}>
                  <div className="text-gray-500 text-xs">
                    {ReasonReact.string(Player.stringOfId(state.me))}
                  </div>
                  <div className="text-gray-500 text-xs">
                    {ReasonReact.string("GameId: " ++ stringOfGameId ++ " ")}
                  </div>
                </div>;
              }}
           </>}
    </div>;
    }
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
