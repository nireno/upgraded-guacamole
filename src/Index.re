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

type mainMenuState = 
| MainMenuDefaultState
| MainMenuReloadState;

type machineState = 
| MainMenuState(mainMenuState)
| ActiveGameState(ClientGame.state)
;

module App = {
  [@react.component]
  let make = () => {
    let url = ReasonReactRouter.useUrl();
    let (maybeSocket, setMaybeSocket) = React.useState(() => None);
    let (notis, updateNotis) = React.useReducer(Noti.State.reducer, Noti.State.initial);
    let (clientSettings, updateClientSettings) = React.useState(() => LocalStorage.getClientSettings());
    let (canJoinPublicGame, updateCanJoinPublicGame) = React.useState(() => true);
    let ( machineState, updateMachineState ) = React.useState(() => MainMenuState(MainMenuDefaultState));

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

    let queryParams = My.Document.location->My.URL.makeURL->My.URL.searchParams;
    let maybeInviteCode = queryParams->My.URL.getSearchParam("g")

    React.useEffect0(
      () => {
        let socket = ClientSocket.T.createWithUrl("/?clientVersion=0.3.0");
        setMaybeSocket(_ => Some(socket));
        ClientSocket.T.on(socket, x => 
          switch (x) {
          | SetState(ioClientState) =>
            switch (ClientGame.state_decode(ioClientState |> Js.Json.parseExn)) {
            | Belt.Result.Error(err) => Js.log(err)
            | Belt.Result.Ok(serverState) =>
              let update = (machineState) => switch(machineState){
              | ActiveGameState(gameState) =>
                let nextGameState = Client.GameReducer.reducer(gameState, MatchServerState(serverState));
                ActiveGameState(nextGameState);
              | _ => 
                // Hinder user from navigating away from an in-progress game.
                Raw.addUnloadListener(Raw.preventUnloadListener);
                ActiveGameState(serverState);
              }
              updateMachineState(update)
            }
          | ShowToast(ioToast) =>
            switch (Noti.t_decode(ioToast |> Js.Json.parseExn)) {
            | Belt.Result.Error(err) => Js.log(err)
            | Belt.Result.Ok(toast) => updateNotis(AddOne(toast))
            }
          | Reset =>
            updateMachineState(machineState =>
              switch (machineState) {
              | ActiveGameState(_) => 
                Raw.removeUnloadListener(Raw.preventUnloadListener);
                MainMenuState(MainMenuDefaultState)
              | otherState => otherState
              }
            );
            updateNotis(Reset(Noti.State.initial));
          | AckOk | AckError(_) => ()
          | HandshakeFailed => 
            updateMachineState(_curr => MainMenuState(MainMenuReloadState));
          }
        );
        switch (maybeInviteCode) {
        | Some(inviteCode) => ReasonReactRouter.replace("./private-games?g=" ++ inviteCode)
        | None => ()
        };
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

    let handleAppClick = _ => {
      /* Clear notifications when user taps anywhere in the app. */
      updateNotis( RemoveKind(Noti.Confirm) );
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
    
    let onExperimentalClick = _event => ReasonReactRouter.replace("./feedback");

    let onExperimentalJoinClick = _event => {
      updateCanJoinPublicGame(_ => false);
      let ackJoinGame = _response => {
        updateCanJoinPublicGame(_ => true);
        ReasonReactRouter.replace("./");
      };
      sendIOWithAck(
        IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
        ackJoinGame,
      );
    };

    let onExperimentalCancelClick = _event => {
      updateCanJoinPublicGame(_ => true);
      ReasonReactRouter.replace("./");
    };

    let onJoinPublicGameClick = _event => {
      updateCanJoinPublicGame(_ => false);
      let ackJoinGame = _response => {
        updateCanJoinPublicGame(_ => true);
      };
      sendIOWithAck(
        IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
        ackJoinGame,
      );
    };

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
            onJoinClick=onExperimentalJoinClick
            canJoinPublicGame
            onCancelClick=onExperimentalCancelClick
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
        <MenuView>
          <JoinPrivateGameView sendJoinGame=sendIoJoinPrivateGame inviteCode=?maybeInviteCode />
        </MenuView>
      </div>;
    | _ => 
    <div
      ref={ReactDOMRe.Ref.domRef(appRef)}
      className="all-fours-game font-sans flex flex-col relative mx-auto"
      onClick=handleAppClick>
      {
        switch(machineState){
        | MainMenuState(state) =>
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
             {
               <button
                 className={"btn btn-blue mt-1 " ++ (canJoinPublicGame ? "" : "btn-disabled")}
                 onClick={node_env == "production" ? onExperimentalClick : onJoinPublicGameClick}
                 disabled={!canJoinPublicGame}>
                 {ReasonReact.string("Join Public Game")}
               </button>;
             }
             <button className="btn btn-blue mt-1 hidden" onClick={_ => ReasonReactRouter.replace("./private-games/")}>
               {ReasonReact.string("Join Private Game")}
             </button>
             <button className="btn btn-blue mt-1" onClick=handleCreatePrivateGameClick>
               {ReasonReact.string("Create Private Game")}
             </button>
             <div className="link link-white mt-4" 
                  onClick={_ => ReasonReactRouter.replace("./settings")}>
               {ReasonReact.string("Settings")}
             </div>
             {switch (Js.Nullable.toOption(allfours_rules_url)) {
              | None => ReasonReact.null
              | Some(allfours_rules_url) =>
                <div
                  className="help absolute w-full flex justify-around"
                  style={ReactDOMRe.Style.make(~bottom="5%", ~height="5%", ())}>
                  <div className="w-1/2 flex flex-col items-center justify-center">
                    <a
                      className="flex flex-col items-center justify-around h-full link-blue" href=allfours_rules_url>
                      <svg className="fill-current w-4" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20">
                        <path d=Icon.question />
                      </svg>
                      <span> {ReasonReact.string("All Fours Rules ")} </span>
                    </a>
                  </div>
                  {switch (Js.Nullable.toOption(allfours_help_url)) {
                   | None => <div className="w-1/2"></div>
                   | Some(allfours_tutorial_url) =>
                     <div className="w-1/2 flex flex-col items-center justify-center">
                       <a
                         className="flex flex-col items-center justify-around h-full link-blue"
                         href=allfours_tutorial_url>
                         <svg className="fill-current w-4" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20">
                           <path d=Icon.question />
                         </svg>
                         <span> {ReasonReact.string("How to play")} </span>
                       </a>
                     </div>
                  }}
                </div>
              }}
              {
                switch(state){
                | MainMenuDefaultState => ReasonReact.null
                | MainMenuReloadState => 
                  <Modal visible=true>
                    <RefreshPrompt />
                  </Modal>
                }
              }
           </MenuView>
        | ActiveGameState(state) =>

          let maybeActivePlayer = Shared.ActivePlayer.find(state.gamePhase, state.dealer);
          let activePlayerName =
            maybeActivePlayer->Belt.Option.mapWithDefault("", activePlayer =>
              switch (state.players->Quad.get(activePlayer.Shared.ActivePlayer.id, _).pla_profile_maybe) {
              | None => ""
              | Some(profile) => profile.client_username
              }
            );

          let (
            (_southName, southCard, southZ, southTags, southIdenticonSeed, southIdenticonStyle, southInitials),
            (_eastName, eastCard, eastZ, eastTags, eastIdenticonSeed, eastIdenticonStyle, eastInitials),
            (_northName, northCard, northZ, northTags, northIdenticonSeed, northIdenticonStyle, northInitials),
            (_westName, westCard, westZ, westTags, westIdenticonSeed, westIdenticonStyle, westInitials),
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
                        
                      let initials = switch(playerId){
                      | N1 => "P1"
                      | N2 => "P2"
                      | N3 => "P3"
                      | N4 => "P4"
                      };
                      
                      let (identiconSeed, identiconStyle) = switch(x.pla_profile_maybe){
                      | None => ("no-profile", "identicon")
                      | Some(profile) => (profile.client_identicon, profile.client_profile_type->ClientSettings.dicebearTypeOfProfileType)
                      };

                      (Player.stringOfId(playerId), x.pla_card, Player.turnDistance(state.leader, playerId), tags, identiconSeed, identiconStyle, initials);
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
          
          let stringOfGameId = SharedGame.stringOfGameId(state.gameId);
          <>
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
                    //  <div className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                    //    style=ReactDOMRe.Style.make(~transform="translateY(1.5em)", ())>
                    //    <div className="text-sm h-5">{ReasonReact.string(westName)}</div>
                    //  </div>
                     <div
                       className="w-1/3 absolute bottom-0 left-0 opacity-80"
                       style={ReactDOMRe.Style.make(~transform="translate(0, 100%)", ())}>
                       <PlayerIdentityView initials=westInitials seed=westIdenticonSeed style=westIdenticonStyle />
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
                       className="player-tags player-tags__north flex flex-row justify-center items-center"
                       tags=northTags
                     />
                     <div
                       className="board-card board-card-north relative self-start flex-shrink-0"
                       style={ReactDOMRe.Style.make(~zIndex=string_of_int(northZ), ())}>
                      //  <div 
                      //    className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                      //    style=ReactDOMRe.Style.make(~transform="translate(-50%, 1.5em)", ())>
                      //    <div className="text-sm h-5">{ReasonReact.string(northName)}</div>
                      //  </div>
                       <div
                         className="w-1/3 absolute top-1/2 -left-1/2 opacity-80"
                         style={ReactDOMRe.Style.make(~transform="translateX(-100%) translateY(-50%)", ())}>
                         <PlayerIdentityView initials=northInitials seed=northIdenticonSeed style=northIdenticonStyle />
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
                         className="absolute w-full top-0 left-0 leading-none"
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
                       <div
                         className="w-1/3 absolute top-1/2 right-0 opacity-80"
                         style={ReactDOMRe.Style.make(~transform="translate(100%, -50%)", ())}>
                         <PlayerIdentityView initials=southInitials seed=southIdenticonSeed style=southIdenticonStyle />
                       </div>
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
                       className="player-tags player-tags__south flex flex-row justify-center items-center"
                       tags=southTags
                     />
                   </div>
                 </div>
                 <div className="game-board__player game-board__player-east">
                   <div
                     className="board-card board-card-east flex-shrink-0"
                     style={ReactDOMRe.Style.make(~zIndex=string_of_int(eastZ), ())}>
                    //  <div className="absolute w-full h-full overflow-hidden flex flex-col justify-end"
                    //    style=ReactDOMRe.Style.make(~transform="translateY(1.5em)", ())>
                    //    <div className="text-sm h-5">{ReasonReact.string(eastName)}</div>
                    //  </div>
                     <div
                       className="w-1/3 absolute top-0 right-0 opacity-80"
                       style={ReactDOMRe.Style.make(~transform="translate(0, -100%)", ())}>
                       <PlayerIdentityView initials=eastInitials seed=eastIdenticonSeed style=eastIdenticonStyle />
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
              | FindPlayersPhase(_) =>
                let (emptySeatCount, canSub) =
                  switch (state.gamePhase) {
                  | FindPlayersPhase({emptySeatCount, canSub}) => (emptySeatCount, canSub)
                  | _ => (0, false)
                  };
                <Modal visible=true>
                  {
                    switch (state.gameId) {
                    | Public(_) =>
                      <FindPlayersView
                        me=state.me
                        players=state.players
                        emptySeatCount
                        canSub
                        onLeaveClick={_event => sendIO(IO_LeaveGame)}
                        onSubClick={_event =>
                          sendIO(
                            IO_Substitute(username, clientSettings->ClientSettings.t_encode->Js.Json.stringify),
                          )
                        }
                      />
                    | Private({private_game_key: key, private_game_host}) => 
                      <InviteFriendsView
                        me=state.me
                        emptySeatCount
                        inviteCode=key
                        onLeaveClick={_event => sendIO(IO_LeaveGame)}
                        players=state.players
                        onGoPublicClick={_event => sendIO(IO_PrivateToPublic)}
                        onRotateGuestsClick={_event => sendIO(IO_RotateGuests)}
                        onStartGameClick={_event => sendIO(IO_TransitionGameNow)}
                        private_game_host
                      />;
                    };
                  }
                </Modal>
              | FindSubsPhase({ emptySeatCount }) => 
                <Modal visible=true> 
                  {
                    switch (state.gameId) {
                    | Private({private_game_key: key, private_game_host}) =>
                      <InviteFriendsView
                        me={state.me}
                        emptySeatCount
                        inviteCode=key
                        onLeaveClick={_event => sendIO(IO_LeaveGame)}
                        players={state.players}
                        onGoPublicClick={_event => sendIO(IO_PrivateToPublic)}
                        onStartGameClick={_event => sendIO(IO_TransitionGameNow)}
                        private_game_host
                      />
                    | Public(_) => 
                      <FindSubsView
                        me={state.me}
                        emptySeatCount
                        onLeaveClick={_event => sendIO(IO_LeaveGame)}
                        players={state.players}
                      />
                    };
                  }
                  
                </Modal>
              | GameOverPhase(rematchDecisions) =>
                <Modal visible=true>
                  <GameOverView
                    me={state.me}
                    players={state.players}
                    rematchDecisions
                    weScore={weTeam.team_score}
                    demScore={demTeam.team_score}
                    playAgainClick={_event => sendIO(IO_Rematch)}
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
           </>
        }
      }
    </div>
    }
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
