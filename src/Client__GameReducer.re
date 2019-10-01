open ClientGame;
type action =
  | MatchServerState(ClientGame.state)

let reducer = (prevState, action) => {
  switch (action) {
  | MatchServerState(nextState) => 
    let stringOfPrevGameId = SharedGame.stringOfGameId(prevState.gameId);
    let stringOfNextGameId = SharedGame.stringOfGameId(nextState.gameId);

    // Prevent user from navigating away from an in-progress game.
    if (stringOfPrevGameId == "" && stringOfNextGameId != "") {
      Raw.addUnloadListener(Raw.preventUnloadListener);
    } else if (stringOfPrevGameId != "" && stringOfNextGameId == "") {
      Raw.removeUnloadListener(Raw.preventUnloadListener);
    } else {
      ();
    };

    let prevNumEmptySeats =
      switch (prevState.gamePhase) {
      | FindPlayersPhase({ emptySeatCount })
      | FindSubsPhase({ emptySeatCount }) => emptySeatCount
      | _ => 0
      };

    let (currHasEmptySeats, currNumEmptySeats) =
      switch (nextState.gamePhase) {
      | FindPlayersPhase({ emptySeatCount})
      | FindSubsPhase({emptySeatCount}) => (true, emptySeatCount)
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

    /** "Game started" sound effect */
    let isGameStarting = 
      switch (prevState.gamePhase) {
      | FindPlayersPhase(_)
      | GameOverPhase(_) =>
        switch (nextState.gamePhase) {
        | DealPhase => true
        | _ => false
        }
      | FindSubsPhase({emptySeatCount: 0}) =>
        switch(nextState.gamePhase){
        | IdlePhase(_)
        | DealPhase
        | BegPhase
        | GiveOnePhase
        | RunPackPhase
        | PlayerTurnPhase(_)
        | PackDepletedPhase => true
        
        | GameOverPhase(_) 
        | FindSubsPhase(_)
        | FindPlayersPhase(_) => false
        }
      | _ => false
      };
    
    isGameStarting
      ? {
        Howler.play(
          Howler.(makeHowl(howlOptions(~src=[|"./static/audio/subtle_start.mp3"|], ()))),
        );
      }
      : ();

    let wasActivePlayer =
      switch (Shared.ActivePlayer.find(prevState.gamePhase, prevState.dealer)) {
      | Some(activePlayer) when activePlayer.id == nextState.me => true
      | _ => false
      };

    let isActivePlayer =
      switch (Shared.ActivePlayer.find(nextState.gamePhase, nextState.dealer)) {
      | Some(activePlayer) when activePlayer.id == nextState.me => true
      | _ => false
      };

    // "player turn" sound effect.
    if (!isGameStarting  //only play if not already playing the "Game starting" sound effect
        && !wasActivePlayer  // only play the first time the player becomes active.
        && isActivePlayer) {
      let sound =
        Howler.(makeHowl(howlOptions(~src=[|"./static/audio/your_turn_subtle.mp3"|], ())));
      Howler.play(sound);
    };

    nextState
  };
};
