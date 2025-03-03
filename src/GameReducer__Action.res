open AppPrelude
open Game

let logger = appLogger.makeChild({"_context": "GameReducer__Action"})

module ValidatePlay = {
  let isPlayerTurn = (~game_phase, ~game_leader_id, ~playerId) => {
    switch game_phase {
    | Game.Phase.PlayerTurnPhase(playerId') => playerId == playerId'
    | BegPhase(BegPhaseStanding) => playerId == game_leader_id
    | _ => false
    }
  }

  type playFailure =
    | WaitForTurn
    | AlreadyPlayed
    | CardNotInHand
    | CantUnderTrump
    | MustFollowSuit
    | MustFollowTrumpedSuit

  let validate = (
    game_phase,
    game_leader_id,
    game_followsuit_maybe,
    pla_hand,
    playerId,
    cardPlayed,
    cardMaybesOnBoard,
    leadSuitMaybe,
    trumpSuit,
  ) => {
    let pla_card = cardMaybesOnBoard->(Quad.get(playerId, _))
    let willUnderTrump = (
      cardPlayed: Card.t,
      pla_hand,
      cardMaybesOnBoard,
      leadSuitMaybe,
      trumpSuit,
    ) =>
      // the card-being-played *is* trump
      cardPlayed.suit == trumpSuit &&
        // the lead-suit is *not* trump
        (leadSuitMaybe->Belt.Option.mapWithDefault(false, suit => suit != trumpSuit) &&
        (// There is a trump card on the board that ranks higher than the card being played
        cardMaybesOnBoard->(
          Quad.exists(
            Belt.Option.mapWithDefault(_, false, ({Card.suit: suit, rank}) =>
              suit == trumpSuit && rank->Card.Rank.intOfRank > cardPlayed.rank->Card.Rank.intOfRank
            ),
            _,
          )
        ) && pla_hand->(List.exists(({Card.suit: suit}) => suit != trumpSuit, _))))
    // The player is still holding a non-trump card

    !isPlayerTurn(~game_phase, ~game_leader_id, ~playerId)
      ? Belt.Result.Error(WaitForTurn)
      : pla_card != None
      ? Belt.Result.Error(AlreadyPlayed)
      : !List.exists(card => card == cardPlayed, pla_hand)
      ? Belt.Result.Error(CardNotInHand)
      : willUnderTrump(cardPlayed, pla_hand, cardMaybesOnBoard, leadSuitMaybe, trumpSuit)
      ? Belt.Result.Error(CantUnderTrump)
      : switch game_followsuit_maybe {
        | Some(suitToFollow)
          if game_leader_id == playerId &&
            (trumpSuit != cardPlayed.Card.suit &&
            cardPlayed.suit != suitToFollow) =>
          Belt.Result.Error(MustFollowTrumpedSuit)
        | _ => Ok()
        }
  }
}

let playCard = (~game_key, ~playerId, ~state: Game.state, ~effects, c) => {
  let logger = logger.makeChild({"_context": "playCard"})
  let player = Quad.get(playerId, state.players)
  let hand' = List.filter(c' => c != c', player.pla_hand)
  let cardMaybesOnBoard = state.players->(Quad.map(player => player.Game.pla_card, _))
  switch state.maybeTrumpCard {
  | None =>
    // This should be an impossible state.
    // No player should be able to play a card when there is no trump on board.
    logger.warn("Player is somehow playing a card when trump is None")
    (state, effects)
  | Some({suit: trumpSuit}) =>
    let validationResult = ValidatePlay.validate(
      state.phase,
      state.leader,
      state.game_follow_suit,
      player.pla_hand,
      playerId,
      c,
      cardMaybesOnBoard,
      state.maybeLeadCard->Belt.Option.map(leadCard => leadCard.suit),
      trumpSuit,
    )

    let updateGame = () => {
      // When the current player is the last player in the trick (i.e. the next player
      // is the lead player), it means this current player will end the trick. There
      // is no need to advance the turn since The true next player will be determined
      // later by computing the trick winner. This test keeps the ui more consistent
      // if the player who wins the trick is the last player in the trick. I can't keep
      // the game in the PlayerTurnPhase since the client needs to know its in a State
      // where the player is not allowed to trigger any events until the timer transitions
      // the game into another active phase.

      let nextPlayer = Quad.nextId(playerId)
      let phase' =
        nextPlayer == state.leader
          ? Game.Phase.IdlePhase(DelayTrickCollection)
          : PlayerTurnPhase(nextPlayer)

      let nextPlayers = Quad.update(
        playerId,
        x => {...x, pla_hand: hand', pla_card: Some(c)},
        state.players,
      )

      let maybeGetTeamJackAward = (
        (maybeCard1, maybeCard2, maybeCard3, maybeCard4),
        maybeLeadCard,
        trumpSuit,
      ) =>
        switch My.Option.all5(maybeCard1, maybeCard2, maybeCard3, maybeCard4, maybeLeadCard) {
        | None => None
        | Some((card1, card2, card3, card4, leadCard)) =>
          let trick = (card1, card2, card3, card4)
          let jackOfTrump = {
            open Card
            {rank: Card.Rank.Jack, suit: trumpSuit}
          }
          let (trickWinnerId, _card) = Trick.getWinnerCard(
            trumpSuit,
            leadCard.Card.suit,
            (card1, card2, card3, card4),
          )
          let trickWinnerTeamId = Game.teamOfPlayer(trickWinnerId)
          switch Quad.getWhere(((_playerId, card)) => card == jackOfTrump, Quad.withId(trick)) {
          | None => None
          | Some((playerId, _card)) =>
            let jackHolderTeamId = Game.teamOfPlayer(playerId)
            jackHolderTeamId == trickWinnerTeamId
              ? Some({
                  GameAward.team_id: jackHolderTeamId,
                  jack_award_type: GameAward.RunJackAward,
                })
              : Some({team_id: trickWinnerTeamId, jack_award_type: HangJackAward})
          }
        }

      let (maybeTeamJackAward, jackAwardNotis) = switch state.maybeTeamJack {
      | None =>
        // name-all-the-things iffy for sets of maybe-items
        let iffyTrick = Quad.map(player => player.pla_card, nextPlayers)
        let maybeTeamJackAward = maybeGetTeamJackAward(iffyTrick, state.maybeLeadCard, trumpSuit)
        let jackAwardNotis = switch maybeTeamJackAward {
        | None => list{}
        | Some({jack_award_type}) =>
          switch jack_award_type {
          | RunJackAward => Noti.broadcast(~msg=Text("Jack gets away!"), ())
          | HangJackAward => Noti.broadcast(~msg=Text("Jack gets hanged!"), ())
          }
        }
        (maybeTeamJackAward, jackAwardNotis)
      | Some(teamJackAward) => (Some(teamJackAward), list{})
      }

      let notiEffects =
        jackAwardNotis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

      (
        {
          ...state,
          players: nextPlayers,
          maybeLeadCard: Js.Option.isNone(state.maybeLeadCard) ? Some(c) : state.maybeLeadCard,
          maybeTeamJack: maybeTeamJackAward,
          phase: phase',
        },
        Belt.List.concat(effects, notiEffects),
      )
    }

    switch validationResult {
    | Belt.Result.Error(validationError) =>
      let noti_message = switch validationError {
      | CardNotInHand => Noti.Text("How are you even playing that card?")
      | CantUnderTrump => Text("You can't under-trump. Play a higher trump or another suit.")
      | MustFollowSuit =>
        switch My.Option.all2(state.maybeLeadCard, state.maybeTrumpCard) {
        | None => Text("You can't play that card.")
        | Some(({suit: leadSuit}, {suit: trumpSuit})) =>
          let leadSuitText = leadSuit->Card.Suit.toString
          let trumpSuitText = trumpSuit->Card.Suit.toString
          Text(`You must follow suit (${leadSuitText}) or play trump (${trumpSuitText}).`)
        }
      | MustFollowTrumpedSuit =>
        switch My.Option.all2(state.maybeTrumpCard, state.game_follow_suit) {
        | None => Text("You can't play that card.")
        | Some(({suit: trumpSuit}, suitToFollow)) =>
          let trumpSuitText = trumpSuit->Card.Suit.toString
          let suitToFollowText = suitToFollow->Card.Suit.toString
          Text(
            `You must follow with the suit you trumped on (${suitToFollowText}) or play trump (${trumpSuitText}).`,
          )
        }
      | WaitForTurn => Text("Wait for your turn.")
      | AlreadyPlayed => Text("You already have a card in play.")
      }

      let errorNoti = {
        Noti.noti_id: Nanoid.nanoid(),
        noti_recipient: playerId,
        noti_message,
        noti_level: Danger,
        noti_kind: Duration(3750),
      }

      let errorNotiEffect = ServerEvent.NotifyPlayer(game_key, errorNoti)

      (state, list{errorNotiEffect, ...effects})

    | Belt.Result.Ok() => updateGame()
    }
  }
}
