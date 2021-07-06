open AppPrelude

@val external node_env: Js.Nullable.t<string> = "process.env.NODE_ENV"
@val external allfours_rules_url: Js.Nullable.t<string> = "process.env.allfours_rules_url"
@val external allfours_help_url: Js.Nullable.t<string> = "process.env.allfours_help_url"

let node_env = node_env |> Js.Nullable.toOption |> Js.Option.getWithDefault("production")

let isProduction = node_env == "production"

@val external username: Js.Nullable.t<string> = "g_display_name"
let username = username |> Js.Nullable.toOption |> Js.Option.getWithDefault("")

open ClientGame

let isPlayerTurn = (turn, playerId) =>
  switch turn {
  | None => false
  | Some(turn) => turn == playerId
  }

type mainMenuState =
  | MainMenuDefaultState
  | MainMenuReloadState

type machineState =
  | MainMenuState(mainMenuState)
  | ActiveGameState(ClientGame.state)

module App = {
  @react.component
  let make = () => {
    let url = ReasonReactRouter.useUrl()
    let (maybeSocket, setMaybeSocket) = React.useState(() => None)
    let (notis, updateNotis) = React.useReducer(Noti.State.reducer, Noti.State.initial)
    let (clientSettings, updateClientSettings) = React.useState(() =>
      LocalStorage.getClientSettings()
    )
    let (canJoinPublicGame, updateCanJoinPublicGame) = React.useState(() => true)
    let (machineState, updateMachineState) = React.useState(() => MainMenuState(
      MainMenuDefaultState,
    ))
    let (sortHand, updateSortHand) = React.useState(() => clientSettings.sort_hand)
    let (signals, updateSignals) = React.useState(() => (None, None, None, None))
    let (signalsEnabled, updateSignalsEnabled) = React.useState(() => true)
    let maybeSignalTimeout = ref(None)

    let saveClientSettings = newClientSettings => {
      LocalStorage.updateClientSettings(newClientSettings)
      updateClientSettings(_ => newClientSettings)
    }

    let (appRect, setAppRect) = React.useState(() =>
      Webapi.Dom.DomRect.make(~x=0.0, ~y=0.0, ~width=0.0, ~height=0.0)
    )

    let appRef: React.Ref.t<Js.Nullable.t<Webapi.Dom.Element.t>> = React.useRef(Js.Nullable.null)

    React.useEffect0(() => {
      let el = My.Nullable.getUnsafe(React.Ref.current(appRef))
      setAppRect(_ => Webapi.Dom.Element.getBoundingClientRect(el))
      None
    })

    let queryParams = My.Document.location->My.URL.makeURL->My.URL.searchParams
    let maybeInviteCode = queryParams->My.URL.getSearchParam("g")

    React.useEffect0(() => {
      let socket = ClientSocket.T.createWithUrl("/?clientVersion=0.6.0")
      setMaybeSocket(_ => Some(socket))
      ClientSocket.T.on(socket, x =>
        switch x {
        | SetState(ioClientState) =>
          switch ClientGame.state_decode(ioClientState |> Js.Json.parseExn) {
          | Belt.Result.Error(err) => Js.log(err)
          | Belt.Result.Ok(serverState) =>
            let update = machineState =>
              switch machineState {
              | ActiveGameState(gameState) =>
                let nextGameState = Client.GameReducer.reducer(
                  gameState,
                  MatchServerState(serverState),
                )
                ActiveGameState(nextGameState)
              | _ =>
                // Hinder user from navigating away from an in-progress game.
                Raw.addUnloadListener(Raw.preventUnloadListener)
                ActiveGameState(serverState)
              }
            updateMachineState(update)
          }
        | ShowToast(ioToast) =>
          switch Noti.t_decode(ioToast |> Js.Json.parseExn) {
          | Belt.Result.Error(err) => Js.log(err)
          | Belt.Result.Ok(toast) => updateNotis(AddOne(toast))
          }
        | Reset =>
          updateMachineState(machineState =>
            switch machineState {
            | ActiveGameState(_) =>
              Raw.removeUnloadListener(Raw.preventUnloadListener)
              MainMenuState(MainMenuDefaultState)
            | otherState => otherState
            }
          )
          updateNotis(Reset(Noti.State.initial))
        | AckOk | AckError(_) => ()
        | HandshakeFailed => updateMachineState(_curr => MainMenuState(MainMenuReloadState))
        | ShowSignal(fromQid, signal) =>
          updateSignals(signals =>
            signals->Quad.update(fromQid, _ => Some((signal, Nanoid.nanoid())), _)
          )

          let timeoutId = Js.Global.setTimeout(
            () => updateSignals(_ => signals->Quad.update(fromQid, _ => None, _)),
            2->secondsToMillis,
          )

          switch maybeSignalTimeout.contents {
          | None => ()
          | Some(timeoutId) => Js.Global.clearTimeout(timeoutId)
          }

          maybeSignalTimeout := Some(timeoutId)
        }
      )
      switch maybeInviteCode {
      | Some(inviteCode) => ReasonReactRouter.replace("./private-games?g=" ++ inviteCode)
      | None => ()
      }
      None
    })

    let sendIO = ioAction =>
      switch maybeSocket {
      | None => ()
      | Some(socket) => ClientSocket.T.emit(socket, ioAction)
      }

    let sendIOWithAck = (ioAction, ack) =>
      switch maybeSocket {
      | None => ()
      | Some(socket) => ClientSocket.T.emitWithAck(socket, ioAction, ack)
      }

    let _createPlayerTricks = tricks =>
      <div className="column">
        {List.length(tricks) == 0
          ? <div> {React.string("No tricks")} </div>
          : <div>
              {List.map(
                trick =>
                  <div key={Trick.stringOfTrick(trick)} className="section"> <Trick trick /> </div>,
                tricks,
              )
              |> Belt.List.toArray
              |> ReasonReact.array}
            </div>}
      </div>

    let handleAppClick = _ =>
      /* Clear notifications when user taps anywhere in the app. */
      updateNotis(RemoveKind(Noti.Confirm))

    let handleCreatePrivateGameClick = _event =>
      sendIO(
        IO_StartPrivateGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
      )

    let sendIoJoinPrivateGame = (inviteCode, ack) =>
      sendIOWithAck(
        IO_JoinPrivateGame(
          inviteCode,
          username,
          ClientSettings.t_encode(clientSettings) |> Js.Json.stringify,
        ),
        ack,
      )

    let onExperimentalClick = _event => ReasonReactRouter.replace("./feedback")

    let onExperimentalJoinClick = _event => {
      updateCanJoinPublicGame(_ => false)
      let ackJoinGame = _response => {
        updateCanJoinPublicGame(_ => true)
        ReasonReactRouter.replace("./")
      }
      sendIOWithAck(
        IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
        ackJoinGame,
      )
    }

    let onExperimentalCancelClick = _event => {
      updateCanJoinPublicGame(_ => true)
      ReasonReactRouter.replace("./")
    }

    let onJoinPublicGameClick = _event => {
      updateCanJoinPublicGame(_ => false)
      let ackJoinGame = _response => updateCanJoinPublicGame(_ => true)
      sendIOWithAck(
        IO_JoinGame(username, ClientSettings.t_encode(clientSettings) |> Js.Json.stringify),
        ackJoinGame,
      )
    }

    let onToggleSortClick = _event => {
      updateSortHand(b => !b)
      open LocalStorage
      setItem(keyToJs(#SortHand), Decco.boolToJson(sortHand)->Js.Json.stringify)
    }

    let onSignalClick = (signal, _event) =>
      signalsEnabled
        ? {
            updateSignalsEnabled(_ => false)
            Js.Global.setTimeout(() => updateSignalsEnabled(_ => true), 350) |> ignore
            sendIO(IO_Signal(signal))
          }
        : ()

    @ocaml.doc(" 
      Reading the url backwords allows the app to work when it isn't served from
      the root url a website i.e. it will work whether the app is served from the
      root url `http://localhost/` or some sub-path like `allfours` in `http://example.com/allfours`
    ")
    switch List.rev(url.path) {
    | list{"feedback", ..._rest} =>
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
      </div>
    | list{"settings", ..._rest} =>
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col justify-center relative mx-auto">
        <MenuView> <SettingsView onSave=saveClientSettings settings=clientSettings /> </MenuView>
      </div>
    | list{"private-games", ..._rest} =>
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col justify-center relative mx-auto">
        <MenuView>
          <JoinPrivateGameView sendJoinGame=sendIoJoinPrivateGame inviteCode=?maybeInviteCode />
        </MenuView>
      </div>
    | _ =>
      <div
        ref={ReactDOMRe.Ref.domRef(appRef)}
        className="all-fours-game font-sans flex flex-col relative mx-auto"
        onClick=handleAppClick>
        {switch machineState {
        | MainMenuState(state) =>
          <MenuView>
            <div
              className="app-name text-white text-5xl"
              style={ReactDOMRe.Style.make(
                ~fontFamily="'Days One', sans-serif",
                ~textShadow="-1px 1px 2px black, 1px -1px 0px #00000080",
                (),
              )}>
              {React.string("All Fours")}
            </div>
            <button
              className={"btn btn-blue mt-1 " ++ (canJoinPublicGame ? "" : "btn-disabled")}
              onClick={node_env == "production" ? onExperimentalClick : onJoinPublicGameClick}
              disabled={!canJoinPublicGame}>
              {React.string("Join Public Game")}
            </button>
            <button
              className="btn btn-blue mt-1 hidden"
              onClick={_ => ReasonReactRouter.replace("./private-games/")}>
              {React.string("Join Private Game")}
            </button>
            <button className="btn btn-blue mt-1" onClick=handleCreatePrivateGameClick>
              {React.string("Create Private Game")}
            </button>
            <div
              className="link link-white mt-4"
              onClick={_ => ReasonReactRouter.replace("./settings")}>
              {React.string("Settings")}
            </div>
            {switch Js.Nullable.toOption(allfours_rules_url) {
            | None => ReasonReact.null
            | Some(allfours_rules_url) =>
              <div
                className="help absolute w-full flex justify-around"
                style={ReactDOMRe.Style.make(~bottom="5%", ~height="5%", ())}>
                <div className="w-1/2 flex flex-col items-center justify-center">
                  <a
                    className="flex flex-col items-center justify-around h-full link-blue"
                    href=allfours_rules_url>
                    <svg
                      className="fill-current w-4"
                      xmlns="http://www.w3.org/2000/svg"
                      viewBox="0 0 20 20">
                      <path d=Icon.question />
                    </svg>
                    <span> {React.string("All Fours Rules ")} </span>
                  </a>
                </div>
                {switch Js.Nullable.toOption(allfours_help_url) {
                | None => <div className="w-1/2" />
                | Some(allfours_tutorial_url) =>
                  <div className="w-1/2 flex flex-col items-center justify-center">
                    <a
                      className="flex flex-col items-center justify-around h-full link-blue"
                      href=allfours_tutorial_url>
                      <svg
                        className="fill-current w-4"
                        xmlns="http://www.w3.org/2000/svg"
                        viewBox="0 0 20 20">
                        <path d=Icon.question />
                      </svg>
                      <span> {React.string("How to play")} </span>
                    </a>
                  </div>
                }}
              </div>
            }}
            {switch state {
            | MainMenuDefaultState => ReasonReact.null
            | MainMenuReloadState => <Modal visible=true> <RefreshPrompt /> </Modal>
            }}
          </MenuView>
        | ActiveGameState(state) =>
          let maybeActivePlayer = Shared.ActivePlayer.find(state.gamePhase, state.dealer)
          // let activePlayerName =
          //   maybeActivePlayer->Belt.Option.mapWithDefault("", activePlayer =>
          //     switch (state.players->Quad.get(activePlayer.Shared.ActivePlayer.id, _).pla_profile_maybe) {
          //     | None => ""
          //     | Some(profile) => profile.client_username
          //     }
          //   );

          let (
            (
              _southName,
              southCard,
              southZ,
              southIsDealer,
              southIsTurner,
              southIdenticonSeed,
              southIdenticonStyle,
              southInitials,
              southSignal,
            ),
            (
              _eastName,
              eastCard,
              eastZ,
              eastIsDealer,
              eastIsTurner,
              eastIdenticonSeed,
              eastIdenticonStyle,
              eastInitials,
              eastSignal,
            ),
            (
              _northName,
              northCard,
              northZ,
              northIsDealer,
              northIsTurner,
              northIdenticonSeed,
              northIdenticonStyle,
              northInitials,
              northSignal,
            ),
            (
              _westName,
              westCard,
              westZ,
              westIsDealer,
              westIsTurner,
              westIdenticonSeed,
              westIdenticonStyle,
              westInitials,
              westSignal,
            ),
          ) = Player.playersAsQuad(~startFrom=state.me, ()) |> Quad.map(playerId =>
            Quad.select(
              playerId,
              x => {
                let isDealer = state.dealer == playerId ? true : false
                let isTurner = switch maybeActivePlayer {
                | None => false
                | Some({id: activePlayerId}) => playerId == activePlayerId ? true : false
                }

                let initials = switch playerId {
                | N1 => "P1"
                | N2 => "P2"
                | N3 => "P3"
                | N4 => "P4"
                }

                let (identiconSeed, identiconStyle) = switch x.pla_profile_maybe {
                | None => ("no-profile", "identicon")
                | Some(profile) => (
                    profile.client_identicon,
                    profile.client_profile_type->ClientSettings.dicebearTypeOfProfileType,
                  )
                }

                let signal = signals->Quad.get(playerId, _)

                (
                  Player.stringOfId(playerId),
                  x.pla_card,
                  Player.turnDistance(state.leader, playerId),
                  isDealer,
                  isTurner,
                  identiconSeed,
                  identiconStyle,
                  initials,
                  signal,
                )
              },
              state.players,
            )
          )

          @ocaml.doc("
          When it is time to remove cards from the board, state.leader should also
          be the trick winner. So this will determine the direction/player the cards
          should animate toward.
          ")
          let animationLeaveTo = switch Player.turnDistance(state.me, state.leader) {
          | 1 => East
          | 2 => North
          | 3 => West
          | _ => South
          }

          let (weTeam, demTeam) = switch teamOfPlayer(state.me) {
          | Team.T1 => (GameTeams.get(T1, state.teams), GameTeams.get(T2, state.teams))
          | Team.T2 => (GameTeams.get(T2, state.teams), GameTeams.get(T1, state.teams))
          }

          let bgBoard = switch maybeActivePlayer {
          | None => " bg-orange-500 "
          | Some({id: activePlayerId}) =>
            state.me == activePlayerId ? " bg-green-500 " : " bg-orange-500 "
          }

          let addUniqueTimoutNoti = msg => {
            let isTimeoutNoti = noti =>
              switch noti.Noti.noti_kind {
              | Duration(_) => true
              | _ => false
              }

            let similarMessages = List.filter(
              noti => isTimeoutNoti(noti) && noti.noti_message == Text(msg),
              notis,
            )
            switch similarMessages {
            | list{} =>
              updateNotis(
                AddOne({
                  open Noti
                  {
                    noti_id: Nanoid.nanoid(),
                    noti_recipient: state.me,
                    noti_level: Danger,
                    noti_kind: Duration(5000),
                    noti_message: Text(msg),
                  }
                }),
              )
            | _ => ()
            }
          }

          let stringOfGameId = SharedGame.stringOfGameId(state.gameId)
          <>
            <div className="trump-card self-center -z-10">
              <GameBoard.CardTransition.PlayCard
                maybeCard=state.maybeTrumpCard enterFrom=North leaveTo=North
              />
            </div>
            <ScoreboardView
              weScore=weTeam.team_score
              wePoints=weTeam.team_points
              demScore=demTeam.team_score
              demPoints=demTeam.team_points
            />
            <div
              className=j`the-rest relative flex-grow flex $bgBoard justify-between`
              style={ReactDOMRe.Style.make(~minHeight="50vh", ())}>
              {
                Belt.List.forEach(notis, notiToRemove =>
                  switch notiToRemove.noti_kind {
                  | Duration(millis) =>
                    Js.Global.setTimeout(() => updateNotis(Remove(notiToRemove)), millis) |> ignore
                  | _ => ()
                  }
                )
                <NotificationsView
                  id="notifications_view" notis appRect teamId={teamOfPlayer(state.me)}
                />
              }
              <div className="west-foe self-center w-1/3">
                <GameBoard__Player
                  zone=West
                  cardLeavesToZone=animationLeaveTo
                  initials=westInitials
                  identiconSeed=westIdenticonSeed
                  identiconStyle=westIdenticonStyle
                  maybeCard=westCard
                  zIndex=westZ
                  isDealer=westIsDealer
                  isTurner=westIsTurner
                  gamePhase=state.gamePhase
                  signal=?westSignal
                />
              </div>
              <div
                className="mid-partners relative self-stretch w-1/3 flex flex-col justify-between">
                {switch state.maybePartnerInfo {
                | None => ReasonReact.null
                | Some(partnerInfo) =>
                  <PlayerCardTagsView
                    className="absolute w-full top-0 left-0 leading-none"
                    style={ReactDOMRe.Style.make(~transform="translate(100%)", ())}
                    cards=partnerInfo.cardsToDisplay
                  />
                }}
                {switch state.maybeTrumpCard {
                | None => ReasonReact.null
                | Some(trumpCard) =>
                  switch state.maybePartnerInfo {
                  | None => ReasonReact.null
                  | Some(partnerInfo) =>
                    <PlayerTrumpsView
                      suit=trumpCard.suit
                      n=partnerInfo.trumpCount
                      className="w-full absolute"
                      style={ReactDOMRe.Style.make(~transform="translateX(-100%)", ())}
                    />
                  }
                }}
                <GameBoard__Player
                  zone=North
                  cardLeavesToZone=animationLeaveTo
                  initials=northInitials
                  identiconSeed=northIdenticonSeed
                  identiconStyle=northIdenticonStyle
                  maybeCard=northCard
                  zIndex=northZ
                  isDealer=northIsDealer
                  isTurner=northIsTurner
                  gamePhase=state.gamePhase
                  signal=?northSignal
                />
                <GameBoard__Player
                  zone=South
                  cardLeavesToZone=animationLeaveTo
                  initials=southInitials
                  identiconSeed=southIdenticonSeed
                  identiconStyle=southIdenticonStyle
                  maybeCard=southCard
                  zIndex=southZ
                  isDealer=southIsDealer
                  isTurner=southIsTurner
                  gamePhase=state.gamePhase
                  signal=?southSignal
                />
              </div>
              <div className="east-foe self-center w-1/3">
                <GameBoard__Player
                  zone=East
                  cardLeavesToZone=animationLeaveTo
                  initials=eastInitials
                  identiconSeed=eastIdenticonSeed
                  identiconStyle=eastIdenticonStyle
                  maybeCard=eastCard
                  zIndex=eastZ
                  isDealer=eastIsDealer
                  isTurner=eastIsTurner
                  gamePhase=state.gamePhase
                  signal=?eastSignal
                />
              </div>
            </div> /* End the-rest */
            <Toolbar onToggleSortClick onSignalClick sortHand />
            <div
              className="player-hand flex flex-col z-20"
              style={ReactDOMRe.Style.make(~gridColumn="1 / 4", ~gridRow="6", ())}>
              <div className="player-hand__placeholder-row flex flex-row justify-around">
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
                <img className="hand-card" src="./static/img/card_placeholder.svg" />
              </div>
              <Hand
                handFacing=state.handFacing
                maybeLeadCard=state.maybeLeadCard
                maybeTrumpCard=state.maybeTrumpCard
                handPhase={switch maybeActivePlayer {
                | None => Hand.FaceUpHand.HandWaitPhase
                | Some(activePlayer) =>
                  activePlayer.id == state.me
                    ? Hand.FaceUpHand.HandPlayPhase
                    : Hand.FaceUpHand.HandWaitPhase
                }}
                sendPlayCard={card =>
                  switch maybeSocket {
                  | None => ()
                  | Some(socket) =>
                    ClientSocket.T.emit(
                      socket,
                      {
                        open SocketMessages
                        IO_PlayCard(
                          Player.id_encode(state.me) |> Js.Json.stringify,
                          Card.t_encode(card) |> Js.Json.stringify,
                        )
                      },
                    )
                  }}
                onInvalidCardClick=addUniqueTimoutNoti
                sortHand
              />
            </div>
            //  <WaitingMessage activePlayerName myPlayerId={state.me} maybeActivePlayer />
            <Player
              sendDeal={_event => sendIO(SocketMessages.IO_Deal)}
              sendStandUp={_event => sendIO(SocketMessages.IO_Stand)}
              sendBeg={_event => sendIO(IO_Beg)}
              sendGiveOne={_event => sendIO(SocketMessages.IO_GiveOne)}
              sendRunPack={_event => sendIO(IO_RunPack)}
              sendReshuffle={_event => sendIO(IO_DealAgain)}
              sendKickFinalTrump={_event => sendIO(IO_FlipFinalTrump)}
              playerPhase=state.phase
            />
            {switch state.gamePhase {
            | FindPlayersPhase(_) =>
              let (emptySeatCount, canSub) = switch state.gamePhase {
              | FindPlayersPhase({emptySeatCount, canSub}) => (emptySeatCount, canSub)
              | _ => (0, false)
              }
              <Modal visible=true>
                {switch state.gameId {
                | Public(_) =>
                  <FindPlayersView
                    me=state.me
                    players=state.players
                    emptySeatCount
                    canSub
                    onLeaveClick={_event => sendIO(IO_LeaveGame)}
                    onSubClick={_event =>
                      sendIO(
                        IO_Substitute(
                          username,
                          clientSettings->ClientSettings.t_encode->Js.Json.stringify,
                        ),
                      )}
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
                  />
                }}
              </Modal>
            | FindSubsPhase({emptySeatCount}) =>
              <Modal visible=true>
                {switch state.gameId {
                | Private({private_game_key: key, private_game_host}) =>
                  <InviteFriendsView
                    me=state.me
                    emptySeatCount
                    inviteCode=key
                    onLeaveClick={_event => sendIO(IO_LeaveGame)}
                    players=state.players
                    onGoPublicClick={_event => sendIO(IO_PrivateToPublic)}
                    onStartGameClick={_event => sendIO(IO_TransitionGameNow)}
                    private_game_host
                  />
                | Public(_) =>
                  <FindSubsView
                    me=state.me
                    emptySeatCount
                    onLeaveClick={_event => sendIO(IO_LeaveGame)}
                    players=state.players
                  />
                }}
              </Modal>
            | GameOverPhase(rematchDecisions) =>
              <Modal visible=true>
                <GameOverView
                  me=state.me
                  players=state.players
                  rematchDecisions
                  weScore=weTeam.team_score
                  demScore=demTeam.team_score
                  playAgainClick={_event => sendIO(IO_Rematch)}
                  leaveClick={_event => sendIO(IO_LeaveGame)}
                />
              </Modal>
            | _ => ReasonReact.null
            }}
            {
              // {createPlayerTricks(state.myTricks)}
              if isProduction {
                ReasonReact.null
              } else {
                <div
                  className="debug-info"
                  style={ReactDOMRe.Style.make(~position="fixed", ~bottom="0", ())}>
                  <div className="text-gray-500 text-xs">
                    {React.string(Player.stringOfId(state.me))}
                  </div>
                  <div className="text-gray-500 text-xs">
                    {React.string("GameId: " ++ (stringOfGameId ++ " "))}
                  </div>
                </div>
              }
            }
          </>
        }}
      </div>
    }
  }
}

ReactDOMRe.renderToElementWithClassName(<App />, "app")
