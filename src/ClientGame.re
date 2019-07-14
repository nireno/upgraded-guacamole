include SharedGame;

[@decco] type maybePlayerId = option(Player.id);
[@decco] type maybeTeamId = option(Team.id);
[@decco] type maybeTeamJackAward = option( (Team.id, GameAward.award) );
[@decco] type maybeCard = option(Card.t);

[@decco]
type playerState = {
  pla_name: string,
  pla_card: option(Card.t),
};

[@decco]
type partnerInfo = {
  trumpCount: int,
  cardsToDisplay: list(Card.t),
};

[@decco]
type state = {
  gameId: string,
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
  maybeTeamHigh: maybeTeamId,
  maybeTeamLow: maybeTeamId,
  maybeTeamJack: maybeTeamJackAward,
  maybeTeamGame: maybeTeamId,
};

let initialState = {
  gameId: "",
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3),
  players: (
    {pla_name: Player.stringOfId(N1), pla_card: None},
    {pla_name: Player.stringOfId(N2), pla_card: None},
    {pla_name: Player.stringOfId(N3), pla_card: None},
    {pla_name: Player.stringOfId(N4), pla_card: None},
  ),
  me: N1,
  maybePartnerInfo: None,
  myTricks: [],
  teams: (initialTeamState, initialTeamState),
  dealer: N1,
  leader: N1,
  handFacing: FaceDownHand(0),
  maybeLeadCard: None,
  maybeTrumpCard: None,
  maybeTeamHigh: None,
  maybeTeamLow: None,
  maybeTeamJack: None,
  maybeTeamGame: None,
};

type action =
  | MatchServerState(state)

let reducer = (prevState, action) => {
  switch (action) {
  | MatchServerState(nextState) => 

    // Prevent user from navigating away from an in-progress game.
    if(prevState.gameId == "" && nextState.gameId != ""){
      Raw.addUnloadListener(Raw.preventUnloadListener);
    } else if(prevState.gameId != "" && nextState.gameId == "") {
      Raw.removeUnloadListener(Raw.preventUnloadListener);
    } else {
      ()
    };

    let (prevHasEmptySeats, prevNumEmptySeats) =
      switch (prevState.gamePhase) {
      | FindPlayersPhase(n)
      | FindSubsPhase(n, _) => (true, n)
      | _ => (false, 0)
      };

    let (currHasEmptySeats, currNumEmptySeats) =
      switch (nextState.gamePhase) {
      | FindPlayersPhase(n)
      | FindSubsPhase(n, _) => (true, n)
      | _ => (false, 0)
      };

    let prevNumCardsOnBoard =
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, prevState.players)
      |> Quad.foldLeft((acc, x) => acc + x);

    let currNumCardsOnBoard =
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, nextState.players)
      |> Quad.foldLeft((acc, x) => acc + x);

    /** "playing a card" sound effect */
    if (currNumCardsOnBoard > prevNumCardsOnBoard) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/play_card.mp3"|])));
      /** Sound needs to Match speed of card animation */ Howler.rate(sound, 0.8);
      Howler.play(sound);
    };

    /** "collecting the trick" sound effect */
    if (prevNumCardsOnBoard == 4 && currNumCardsOnBoard == 0) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/play_card.mp3"|])));
      Howler.play(sound);
    };

    /** "player left the game" sound effect */
    if (currHasEmptySeats && currNumEmptySeats > prevNumEmptySeats) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/player_left.mp3"|])));
      Howler.play(sound);
    };

    /** "player joined the game" sound effect */
    if (currHasEmptySeats && currNumEmptySeats < prevNumEmptySeats) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/player_joined.mp3"|])));
      Howler.play(sound);
    };

    /** "Game in progress" sound effect */
    if (prevHasEmptySeats && currNumEmptySeats == 0) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/subtle_start.mp3"|])));
      Howler.play(sound);
    };

    let isPlayerTurn =
      switch (ActivePlayer.find(nextState.gamePhase, nextState.dealer)) {
      | Some(activePlayer) when activePlayer.id == nextState.me => true
      | _ => false
      };

    /** "player turn" sound effect.  This is only played if not already playing the "Game in progress" sound effect */
    if (!prevHasEmptySeats && isPlayerTurn) {
      let sound = Howler.(makeHowl(options(~src=[|"./static/audio/your_turn_subtle.mp3"|])));
      Howler.play(sound);
    };

    nextState
  };
};
