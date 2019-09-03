include SharedGame;

[@decco] type maybeCard = option(Card.t);

[@decco] type maybeTeamHigh = option(GameAward.luckyAwardData);
[@decco] type maybeTeamLow = option(GameAward.luckyAwardData);
[@decco] type maybeTeamJack = option(GameAward.jackAwardData);
[@decco] type maybeTeamGame = option(GameAward.gameAwardData);

[@decco]
type playerState = {
  pla_name: string,
  pla_card: option(Card.t),
};

let initPlayerState = playerId => {
  pla_name: Player.stringOfId(playerId),
  pla_card: None,
};

[@decco]
type partnerInfo = {
  trumpCount: int,
  cardsToDisplay: list(Card.t),
};

[@decco]
type state = {
  gameId: game_id,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  players: (playerState, playerState, playerState, playerState),
  me: Player.id,
  maybePartnerInfo: option(partnerInfo),
  myTricks: list(Trick.t),
  teams: (teamState, teamState),
  dealer: Player.id,
  leader: Player.id,
  handFacing: Hand.handFacing,
  maybeLeadCard: maybeCard,
  maybeTrumpCard: maybeCard,
};

let initialState = {
  gameId: Public(""),
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3, false),
  players: Quad.make(initPlayerState),
  me: N1,
  maybePartnerInfo: None,
  myTricks: [],
  teams: (initialTeamState, initialTeamState),
  dealer: N1,
  leader: N1,
  handFacing: FaceDownHand(0),
  maybeLeadCard: None,
  maybeTrumpCard: None,
};

type action =
  | MatchServerState(state)

let reducer = (prevState, action) => {
  switch (action) {
  | MatchServerState(nextState) => 
    let stringOfPrevGameId = stringOfGameId(prevState.gameId);
    let stringOfNextGameId = stringOfGameId(nextState.gameId);

    // Prevent user from navigating away from an in-progress game.
    if (stringOfPrevGameId == "" && stringOfNextGameId != "") {
      Raw.addUnloadListener(Raw.preventUnloadListener);
    } else if (stringOfPrevGameId != "" && stringOfNextGameId == "") {
      Raw.removeUnloadListener(Raw.preventUnloadListener);
    } else {
      ();
    };

    let (prevHasEmptySeats, prevNumEmptySeats) =
      switch (prevState.gamePhase) {
      | FindPlayersPhase(n, _)
      | FindSubsPhase(n, _) => (true, n)
      | _ => (false, 0)
      };

    let (currHasEmptySeats, currNumEmptySeats) =
      switch (nextState.gamePhase) {
      | FindPlayersPhase(n, _)
      | FindSubsPhase(n, _) => (true, n)
      | _ => (false, 0)
      };

    let prevNumCardsOnBoard =
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, prevState.players)
      |> Quad.foldLeft((acc, x) => acc + x);

    let currNumCardsOnBoard =
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, nextState.players)
      |> Quad.foldLeft((acc, x) => acc + x);
    
    let clientSettings = LocalStorage.getClientSettings()
    let volume = switch(clientSettings.volume){
    | Mute(_) => 0.0
    | Level(v) => v
    };

    let howlOptions = Howler.options(~volume);
    /** "playing a card" sound effect */
    if (currNumCardsOnBoard > prevNumCardsOnBoard) {
      let sound = Howler.(makeHowl(howlOptions(~src=[|"./static/audio/play_card.mp3"|], ())));
      /** Sound needs to Match speed of card animation */ Howler.rate(sound, 0.8);
      Howler.play(sound);
    };

    /** "collecting the trick" sound effect */
    if (prevNumCardsOnBoard == 4 && currNumCardsOnBoard == 0) {
      let sound = Howler.(makeHowl(howlOptions(~src=[|"./static/audio/play_card.mp3"|], ())));
      Howler.play(sound);
    };

    /** "player left the game" sound effect */
    if (currHasEmptySeats && currNumEmptySeats > prevNumEmptySeats) {
      let sound = Howler.(makeHowl(howlOptions(~src=[|"./static/audio/player_left.mp3"|], ())));
      Howler.play(sound);
    };

    /** "player joined the game" sound effect */
    if (currHasEmptySeats && currNumEmptySeats < prevNumEmptySeats) {
      let sound = Howler.(makeHowl(howlOptions(~src=[|"./static/audio/player_joined.mp3"|], ())));
      Howler.play(sound);
    };

    /** "Game in progress" sound effect */
    if (prevHasEmptySeats && currNumEmptySeats == 0) {
      let sound = Howler.(makeHowl(howlOptions(~src=[|"./static/audio/subtle_start.mp3"|], ())));
      Howler.play(sound);
    };

    let wasActivePlayer =
      switch (ActivePlayer.find(prevState.gamePhase, prevState.dealer)) {
      | Some(activePlayer) when activePlayer.id == nextState.me => true
      | _ => false
      };

    let isActivePlayer =
      switch (ActivePlayer.find(nextState.gamePhase, nextState.dealer)) {
      | Some(activePlayer) when activePlayer.id == nextState.me => true
      | _ => false
      };

    // "player turn" sound effect.
    if (!prevHasEmptySeats  //only play if not already playing the "Game in progress" sound effect
        && !wasActivePlayer  // only play the first time the player becomes active.
        && isActivePlayer) {
      let sound =
        Howler.(makeHowl(howlOptions(~src=[|"./static/audio/your_turn_subtle.mp3"|], ())));
      Howler.play(sound);
    };

    nextState
  };
};
