open ClientGame
type action = MatchServerState(ClientGame.state)

let reducer = (prevState, action) =>
  switch action {
  | MatchServerState(nextState) =>
    let prevNumEmptySeats = switch prevState.gamePhase {
    | FindPlayersPhase({emptySeatCount})
    | FindSubsPhase({emptySeatCount}) => emptySeatCount
    | _ => 0
    }

    let (currHasEmptySeats, currNumEmptySeats) = switch nextState.gamePhase {
    | FindPlayersPhase({emptySeatCount})
    | FindSubsPhase({emptySeatCount}) => (true, emptySeatCount)
    | _ => (false, 0)
    }

    let prevNumCardsOnBoard = Quad.foldLeft(
      (acc, x) => acc + x,
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, prevState.players),
    )

    let currNumCardsOnBoard = Quad.foldLeft(
      (acc, x) => acc + x,
      Quad.map(player => Js.Option.isSome(player.pla_card) ? 1 : 0, nextState.players),
    )

    let clientSettings = LocalStorage.getClientSettings()
    let volume = switch clientSettings.volume {
    | Mute(_) => 0.0
    | Level(v) => v
    }

    let howlOptions = Howler.options(~volume)

    @ocaml.doc(" \"playing a card\" sound effect ")
    if currNumCardsOnBoard > prevNumCardsOnBoard {
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/play_card.mp3"], ()))
      }

      @ocaml.doc(" Sound needs to Match speed of card animation ")
      Howler.rate(sound, 0.8)
      Howler.play(sound)
    }

    @ocaml.doc(" \"collecting the trick\" sound effect ")
    if prevNumCardsOnBoard == 4 && currNumCardsOnBoard == 0 {
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/play_card.mp3"], ()))
      }
      Howler.play(sound)
    }

    @ocaml.doc(" \"player left the game\" sound effect ")
    if currHasEmptySeats && currNumEmptySeats > prevNumEmptySeats {
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/player_left.mp3"], ()))
      }
      Howler.play(sound)
    }

    @ocaml.doc(" \"player joined the game\" sound effect ")
    if currHasEmptySeats && currNumEmptySeats < prevNumEmptySeats {
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/player_joined.mp3"], ()))
      }
      Howler.play(sound)
    }

    @ocaml.doc(" \"Game started\" sound effect ")
    let isGameStarting = switch prevState.gamePhase {
    | FindPlayersPhase(_)
    | GameOverPhase(_) =>
      switch nextState.gamePhase {
      | DealPhase => true
      | _ => false
      }
    | FindSubsPhase({emptySeatCount: 0}) =>
      switch nextState.gamePhase {
      | IdlePhase(_)
      | DealPhase
      | BegPhase(_)
      | GiveOnePhase
      | RunPackPhase
      | FlipFinalTrumpPhase
      | PlayerTurnPhase(_)
      | PackDepletedPhase => true

      | GameOverPhase(_)
      | FindSubsPhase(_)
      | FindPlayersPhase(_) => false
      }
    | _ => false
    }

    isGameStarting
      ? Howler.play({
          open Howler
          makeHowl(howlOptions(~src=["./static/audio/subtle_start.mp3"], ()))
        })
      : ()

    let wasActivePlayer = switch Shared.ActivePlayer.find(prevState.gamePhase, prevState.dealer) {
    | Some(activePlayer) if activePlayer.id == nextState.me => true
    | _ => false
    }

    let isActivePlayer = switch Shared.ActivePlayer.find(nextState.gamePhase, nextState.dealer) {
    | Some(activePlayer) if activePlayer.id == nextState.me => true
    | _ => false
    }

    // "player turn" sound effect.
    if !isGameStarting && (!wasActivePlayer && isActivePlayer) {
      //only play if not already playing the "Game starting" sound effect // only play the first time the player becomes active.
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/your_turn_subtle.mp3"], ()))
      }
      Howler.play(sound)
    }

    /* notify-player-can-sub sound effect */
    let getCanSub = x =>
      switch x {
      | FindPlayersPhase({canSub}) => canSub
      | _ => false
      }

    if getCanSub(prevState.gamePhase) == false && getCanSub(nextState.gamePhase) == true {
      let sound = {
        open Howler
        makeHowl(howlOptions(~src=["./static/audio/player_joined.mp3"], ()))
      }
      Howler.play(sound)
    }

    nextState
  }
