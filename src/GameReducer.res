open AppPrelude
open Game

let logger = appLogger.makeChild({"_context": "GameReducer"})

let getTeamHighAndLowMaybes: (
  (Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t, Hand.FaceUpHand.t),
  option<Card.t>,
) => (option<AllFours.GameAward.luckyAwardData>, option<AllFours.GameAward.luckyAwardData>) = (
  (player1Hand, player2Hand, player3Hand, player4Hand),
  maybeTrumpCard,
) =>
  switch maybeTrumpCard {
  | None => (None, None)
  | Some({Card.suit: trumpSuit}) =>
    let playerCards = List.flatten(list{
      player1Hand->Belt.List.map(card => (Quad.N1, card)),
      player2Hand->Belt.List.map(card => (Quad.N2, card)),
      player3Hand->Belt.List.map(card => (Quad.N3, card)),
      player4Hand->Belt.List.map(card => (Quad.N4, card)),
    })

    let playersTrumpAsc: list<(Player.id, Card.t)> =
      playerCards
      |> List.filter(((_, {Card.suit: suit})) => suit == trumpSuit)
      |> List.sort(((_, {Card.rank: rank1}), (_, {rank: rank2})) => compare(rank1, rank2))

    let playersTrumpDesc = List.rev(playersTrumpAsc)

    let getTeamHeadCardMaybe = (team_id, playerCards) =>
      switch playerCards
      ->Belt.List.keep(((player_id, _card)) => player_id->teamOfPlayer == team_id)
      ->Belt.List.head {
      | None => None
      | Some((_, card)) => Some(card)
      }

    let maybeTeam1High = getTeamHeadCardMaybe(T1, playersTrumpDesc)
    let maybeTeam2High = getTeamHeadCardMaybe(T2, playersTrumpDesc)
    let maybeTeam1Low = getTeamHeadCardMaybe(T1, playersTrumpAsc)
    let maybeTeam2Low = getTeamHeadCardMaybe(T2, playersTrumpAsc)

    let maybeTeamLuckyAward = (maybeTeam1Card, maybeTeam2Card, cmp) =>
      switch (maybeTeam1Card, maybeTeam2Card) {
      | (None, None) => None
      | (None, Some(card)) =>
        Some({
          open GameAward
          {team_id: T2, winning_card: card, losing_card_maybe: None}
        })
      | (Some(card), None) => Some({team_id: T1, winning_card: card, losing_card_maybe: None})
      | (Some(card1), Some(card2)) =>
        cmp(card1.rank->Card.Rank.intOfRank, card2.rank->Card.Rank.intOfRank)
          ? Some({team_id: T1, winning_card: card1, losing_card_maybe: Some(card2)})
          : Some({team_id: T2, winning_card: card2, losing_card_maybe: Some(card1)})
      }

    let maybeTeamHighAward = maybeTeamLuckyAward(maybeTeam1High, maybeTeam2High, \">")
    let maybeTeamLowAward = maybeTeamLuckyAward(maybeTeam1Low, maybeTeam2Low, \"<")

    (maybeTeamHighAward, maybeTeamLowAward)
  }

let maybeAddHighPoint = ((state, awards)) =>
  switch state.maybeTeamHigh {
  | None => (state, awards)
  | Some({team_id} as highAwardData) =>
    let state = {
      ...state,
      teams: GameTeams.update(
        team_id,
        x => {...x, team_score: x.team_score + Ruleset.default.highAwardValue},
        state.teams,
      ),
    }
    let (_, l, j, g) = awards
    (state, (Some(highAwardData), l, j, g))
  }

let maybeAddLowPoint = ((state, awards)) =>
  switch state.maybeTeamLow {
  | None => (state, awards)
  | Some({team_id} as lowAwardData) =>
    let state = {
      ...state,
      teams: GameTeams.update(
        team_id,
        x => {...x, team_score: x.team_score + Ruleset.default.lowAwardValue},
        state.teams,
      ),
    }
    let (h, _, j, g) = awards
    (state, (h, Some(lowAwardData), j, g))
  }

let maybeAddJackPoints = ((state, awards)) =>
  switch state.maybeTeamJack {
  | None => (state, awards)
  | Some({team_id, jack_award_type} as jackAwardData) =>
    let state = {
      ...state,
      teams: GameTeams.update(
        team_id,
        x => {...x, team_score: x.team_score + jack_award_type->GameAward.jackAwardValue},
        state.teams,
      ),
    }
    let (h, l, _, g) = awards
    (state, (h, l, Some(jackAwardData), g))
  }

let isCardInTrick = card => Quad.exists(c => c == card, _)
let isCardInPlayerTricks = (card, player) => player.pla_tricks |> List.exists(isCardInTrick(card))

let isCardInGameTrick = (state, card) => state.players->Quad.exists(isCardInPlayerTricks(card), _)

let isHighTrumpInGameTrick = state =>
  switch state.maybeTeamHigh {
  | Some({winning_card}) if isCardInGameTrick(state, winning_card) => true
  | _ => false
  }

let isLowTrumpInGameTrick = state =>
  switch state.maybeTeamLow {
  | Some({winning_card}) if isCardInGameTrick(state, winning_card) => true
  | _ => false
  }

let getActiveScoreMods = state => {
  let isHighTrumpInGameTrick = isHighTrumpInGameTrick(state)
  isHighTrumpInGameTrick && isLowTrumpInGameTrick(state)
    ? list{maybeAddHighPoint, maybeAddLowPoint, maybeAddJackPoints}
    : isHighTrumpInGameTrick
    ? list{maybeAddHighPoint}
    : list{}
}

let getKickTrumpNotis = maybeTrumpCard =>
  switch maybeTrumpCard {
  | Some(trumpCard) if kickPoints(trumpCard.Card.rank) > 0 =>
    let trumpCardText = trumpCard->Card.stringOfCard
    let kickPointsText = kickPoints(trumpCard.rank)->string_of_int
    let msg = j`Dealer kicked $trumpCardText:  +$kickPointsText`
    Noti.broadcast(~msg=Text(msg), ~kind=Confirm, ())
  | _ => list{}
  }

module ValidatePlay = {
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
    let pla_card = cardMaybesOnBoard->Quad.get(playerId, _)
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
        cardMaybesOnBoard->Quad.exists(
          Belt.Option.mapWithDefault(_, false, ({Card.suit: suit, rank}) =>
            suit == trumpSuit && rank->Card.Rank.intOfRank > cardPlayed.rank->Card.Rank.intOfRank
          ),
          _,
        ) && pla_hand->List.exists(({Card.suit: suit}) => suit != trumpSuit, _)))
    // The player is still holding a non-trump card

    game_phase != PlayerTurnPhase(playerId)
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

/* Handle things that take effect only when leaving a particular state */
let addLeaveStateEffects = (statePrev, (stateNext, effects)) =>
  statePrev.phase == stateNext.phase
    ? (stateNext, effects)
    : {
        let game_key = statePrev.game_id->SharedGame.stringOfGameId
        switch statePrev.phase {
        | GameOverPhase(rematchDecisions) if Game.isRematchAcceptedByAll(rematchDecisions) => (
            stateNext,
            Belt.List.concat(effects, list{ServerEvent.DiscardGameTimer(game_key)}),
          )
        | FindPlayersPhase({emptySeatCount: 0}) as phase
        | FindSubsPhase({emptySeatCount: 0}) as phase
        | IdlePhase(DelayTrickCollection) as phase
        | phase
          if phase->Game.isPlayerActivePhase => /* At the moment I assume there is only 1 concurrent timer in effect for a game
           at any given moment. So If I'm leaving a phase that sets up a timer,
           I should clean up afterwards */
          (stateNext, Belt.List.concat(effects, list{ServerEvent.DiscardGameTimer(game_key)}))
        | _ => (stateNext, effects)
        }
      }

let addEnterStateEffects = (statePrev, (stateNext, effects)) =>
  statePrev.phase == stateNext.phase
    ? (stateNext, effects)
    : {
        let game_key = statePrev.game_id->SharedGame.stringOfGameId
        switch stateNext.phase {
        | IdlePhase(DelayTrickCollection) => (
            stateNext,
            Belt.List.concat(
              effects,
              list{ServerEvent.CreateGameTimer(game_key, DelayedGameEvent(AdvanceRound, 2750))},
            ),
          )
        | GameOverPhase(rematchDecisions) if Game.isRematchAcceptedByAll(rematchDecisions) => (
            stateNext,
            Belt.List.concat(
              effects,
              list{
                ServerEvent.CreateGameTimer(
                  game_key,
                  DelayedGameEvent(
                    StartRematch,
                    SharedGame.settings.gameStartingCountdownSeconds->secondsToMillis,
                  ),
                ),
              },
            ),
          )
        | phase if phase->Game.isPlayerActivePhase => (
            stateNext,
            Belt.List.concat(
              effects,
              list{ServerEvent.CreateGameTimer(game_key, KickInactiveClientCountdown)},
            ),
          )
        | _ => (stateNext, effects)
        }
      }

/* Transfers dealing responsibility to the next player on the losing team. */
let getRematchDealerId = (currDealerId, teams) => {
  let ({team_score: team1Score}, {team_score: team2Score}) = teams
  let winningTeamId = team1Score >= team2Score ? Team.T1 : T2
  let nextToDealerId = Quad.nextId(currDealerId)
  let currDealerPartnerId = Player.getPartner(currDealerId)
  let teamOfDealer = teamOfPlayer(currDealerId)
  teamOfDealer == winningTeamId ? nextToDealerId : currDealerPartnerId
}

let initializeRematch = (game_id, rematchClients, rematchPhase, rematchDealerId) => {
  ...Game.initialState(),
  game_id,
  players: Quad.make(_ => Game.initPlayerData()),
  dealer: rematchDealerId,
  clients: rematchClients,
  phase: rematchPhase,
}

/*
  This module should probably be called GameMachine and this function should
  be the gameState machine. It takes some (before) state and an action and
  produces some (after) state state.
*/
let reduce = (action, state) => {
  let game_key = state.game_id->SharedGame.stringOfGameId

  let rec reduceRec: (
    Game.event,
    Game.state,
    list<ServerEvent.effect>,
  ) => (Game.state, list<ServerEvent.effect>) = (action, state, effects) =>
    switch action {
    | Noop => (state, effects)
    | NewRound =>
      /* what used to be EndRound start */
      let team1Tricks = Belt.List.concat(
        Quad.get(N1, state.players).pla_tricks,
        Quad.get(N3, state.players).pla_tricks,
      )
      let team2Tricks = Belt.List.concat(
        Quad.get(N2, state.players).pla_tricks,
        Quad.get(N4, state.players).pla_tricks,
      )

      let calcPoints = tricks =>
        tricks
        |> List.map(Quad.toList)
        |> List.concat
        |> List.fold_left((acc, {Card.rank: rank}) => acc + Card.Rank.pointsOfRank(rank), 0)

      let team1Points = calcPoints(team1Tricks)
      let team2Points = calcPoints(team2Tricks)

      let maybeTeamGame =
        team1Points == team2Points
          ? Some({
              GameAward.team_id_maybe: None,
              winning_count: team1Points,
              losing_count: team2Points,
            })
          : team1Points > team2Points
          ? Some({
            GameAward.team_id_maybe: Some(Team.T1),
            winning_count: team1Points,
            losing_count: team2Points,
          })
          : Some({
              team_id_maybe: Some(Team.T2),
              winning_count: team2Points,
              losing_count: team1Points,
            })

      let maybeAddGamePoint = (maybeTeamGame: option<GameAward.gameAwardData>, (state, awards)) =>
        switch maybeTeamGame {
        | Some({GameAward.team_id_maybe: Some(T1 as team_id)} as gameAwardData)
        | Some({GameAward.team_id_maybe: Some(T2 as team_id)} as gameAwardData) =>
          let state = {
            ...state,
            teams: GameTeams.update(
              team_id,
              x => {...x, team_score: x.team_score + Ruleset.default.gameAwardValue},
              state.teams,
            ),
          }
          let (h, l, j, _) = awards
          (state, (h, l, j, Some(gameAwardData)))
        | Some(gameAwardData) =>
          let (h, l, j, _) = awards
          (state, (h, l, j, Some(gameAwardData)))
        | _ => (state, awards)
        }

      let (state, (h, l, j, g)) = Util.updateUntil(
        list{
          maybeAddHighPoint,
          maybeAddLowPoint,
          maybeAddJackPoints,
          maybeAddGamePoint(maybeTeamGame),
        },
        ((state, _)) => isGameOverTest(state),
        (state, (None, None, None, None)),
      )

      let nextDealer = Quad.nextId(state.dealer)

      let notiEffects = Noti.broadcast(
        ~msg=RoundSummary({
          noti_maybeTeamHigh: h,
          noti_maybeTeamLow: l,
          noti_maybeTeamJack: j,
          noti_maybeTeamGame: g,
        }),
        ~kind=Confirm,
        (),
      )->Belt.List.map(noti => ServerEvent.NotifyPlayer(
        state.game_id->SharedGame.stringOfGameId,
        noti,
      ))

      let effects = effects->Belt.List.concat(notiEffects)

      (
        {
          ...state,
          phase: isGameOverTest(state) ? GameOverPhase(Quad.make(_ => RematchUnknown)) : DealPhase,
          deck: Deck.make() |> Deck.shuffle,
          players: Quad.map(x => {...x, pla_tricks: list{}}, state.players),
          dealer: nextDealer,
          maybeTrumpCard: None,
          maybeLeadCard: None,
          maybeTeamHigh: None,
          maybeTeamLow: None,
          maybeTeamJack: None,
        },
        effects,
      )

    | PlayCard(playerId, c) =>
      switch state.phase {
      | PlayerTurnPhase(phasePlayerId) if phasePlayerId == playerId =>
        let player = Quad.get(playerId, state.players)
        let hand' = List.filter(c' => c != c', player.pla_hand)
        let cardMaybesOnBoard = state.players->Quad.map(player => player.pla_card, _)
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
            /*
              When the current player is the last player in the trick (i.e. the next player
              is the lead player), it means this current player will end the trick. There
              is no need to advance the turn since The true next player will be determined
              later by computing the trick winner. This test keeps the ui more consistent
              if the player who wins the trick is the last player in the trick. I can't keep
              the game in the PlayerTurnPhase since the client needs to know its in a State
              where the player is not allowed to trigger any events until the timer transitions 
              the game into another active phase.
 */
            let nextPlayer = Quad.nextId(playerId)
            let phase' =
              nextPlayer == state.leader
                ? IdlePhase(DelayTrickCollection)
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
                switch Quad.withId(trick) |> Quad.getWhere(((_playerId, card)) =>
                  card == jackOfTrump
                ) {
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
              let maybeTeamJackAward = maybeGetTeamJackAward(
                iffyTrick,
                state.maybeLeadCard,
                trumpSuit,
              )
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
                maybeLeadCard: Js.Option.isNone(state.maybeLeadCard)
                  ? Some(c)
                  : state.maybeLeadCard,
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
                Text(j`You must follow suit ($leadSuit) or play trump ($trumpSuit).`)
              }
            | MustFollowTrumpedSuit =>
              switch My.Option.all2(state.maybeTrumpCard, state.game_follow_suit) {
              | None => Text("You can't play that card.")
              | Some(({suit: trumpSuit}, suitToFollow)) =>
                let trumpSuitText = trumpSuit->Card.Suit.toString
                let suitToFollowText = suitToFollow->Card.Suit.toString
                Text(j`You must follow with the suit you trumped on ($suitToFollowText) or play trump ($trumpSuitText).`)
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

      | _ =>
        let playerIdText = playerId->Player.stringOfId
        logger.warn(`PlayerTurnPhase(${playerIdText}) recieved out of phase.`)
        (state, effects)
      }

    | Deal =>
      switch state.phase {
      | DealPhase =>
        let dealCards = state => {
          let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, state.deck)
          let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
          let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
          let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
          {
            ...state,
            deck,
            leader: Quad.nextId(state.dealer),
            players: state.players
            |> Quad.update(N1, x => {...x, pla_hand: p1Hand})
            |> Quad.update(N2, x => {...x, pla_hand: p2Hand})
            |> Quad.update(N3, x => {...x, pla_hand: p3Hand})
            |> Quad.update(N4, x => {...x, pla_hand: p4Hand}),
            teams: GameTeams.map(x => {...x, team_points: 0}, state.teams),
          }
        }

        let kickTrump = state => {
          let dealerTeam = teamOfPlayer(state.dealer)
          let (cards, deck) = Deck.deal(1, state.deck)
          let trumpCard = List.hd(cards) /* Dealing expects enough cards to kick trump. #unsafe */
          let points = kickPoints(trumpCard.rank)

          {
            ...state,
            deck,
            maybeTrumpCard: Some(trumpCard),
            teams: GameTeams.update(
              dealerTeam,
              x => {...x, team_score: x.team_score + points},
              state.teams,
            ),
          }
        }

        let state = state |> dealCards |> kickTrump

        let kickTrumpNotiEffects =
          getKickTrumpNotis(state.maybeTrumpCard)->Belt.List.map(noti => ServerEvent.NotifyPlayer(
            game_key,
            noti,
          ))

        (
          {
            ...state,
            phase: isGameOverTest(state) ? GameOverPhase(Quad.make(_ => RematchUnknown)) : BegPhase,
          },
          Belt.List.concat(kickTrumpNotiEffects, effects),
        )

      | _ =>
        logger.warn("`Deal` recieved out of phase.")
        (state, effects)
      }

    | AdvanceRound =>
      let (p1CardMaybe, p2CardMaybe, p3CardMaybe, p4CardMaybe) =
        state.players->Quad.map(p => p.pla_card, _)

      switch My.Option.all6(
        p1CardMaybe,
        p2CardMaybe,
        p3CardMaybe,
        p4CardMaybe,
        state.maybeLeadCard,
        state.maybeTrumpCard,
      ) {
      | None => (state, effects)
      | Some((p1Card, p2Card, p3Card, p4Card, {Card.suit: leadSuit}, {Card.suit: trumpSuit})) =>
        let trick = (p1Card, p2Card, p3Card, p4Card)

        let (trickWinner, trickWinnerCard) = Trick.getWinnerCard(trumpSuit, leadSuit, trick)

        let game_follow_suit = switch state.game_follow_suit {
        // clear the follow-suit field whenever trick-winner is not the leader
        | Some(_) if state.leader != trickWinner => None
        // the above case clears the follow-suit field in all cases when the trick-winner
        // is not the leader. The only remaining case when game_follow_suit is Some(thing)
        // would be when the trick-winner *is* the leader and in this case I only None the follow-suit
        // flag if the leader did indeed follow with the suit-to-follow
        | Some(suitToFollow) if trickWinnerCard.suit == suitToFollow => None
        | game_followsuit_maybe =>
          // in order for trump-and-follow to be but in effect
          // the leading-suit *must not* be trump
          leadSuit != trumpSuit &&
            // the player must win the trick with trump
            (trickWinnerCard.suit == trumpSuit &&
            // and must still have a leading-suit card in hand
            Quad.select(
              trickWinner,
              p => p.pla_hand->List.exists(({Card.suit: suit}) => suit == leadSuit, _),
              state.players,
            ))
            ? Some(leadSuit)
            : game_followsuit_maybe
        }

        // Commented out the notifications because it might get annoying for people
        // who already accustomed to trump-and-follow.
        // let followSuitNotis =
        //   game_follow_suit->Belt.Option.mapWithDefault(
        //     [],
        //     capturedSuit => {
        //       let trumpSuitText = trumpSuit->Card.Suit.toString;
        //       let capturedSuitText = capturedSuit->Card.Suit.toString;
        //       [
        //         {
        //           Noti.noti_id: Nanoid.nanoid(),
        //           noti_recipient: trickWinner,
        //           noti_message:
        //             Text(
        //               {j|You must follow with the captured suit ($capturedSuitText) or play trump ($trumpSuitText)|j},
        //             ),
        //           noti_level: Warning,
        //           noti_kind: Confirm,
        //         },
        //       ];
        //     },
        //   );
        let advanceRound = state => {
          ...state,
          players: Quad.update(
            trickWinner,
            x => {...x, pla_tricks: Belt.List.concat(x.pla_tricks, list{trick})},
            state.players,
          ) |> Quad.map(x => {...x, pla_card: None}),
          teams: GameTeams.update(
            teamOfPlayer(trickWinner),
            x => {...x, team_points: x.team_points + Trick.getValue(trick)},
            state.teams,
          ),
          leader: trickWinner,
          maybeLeadCard: None,
          phase: PlayerTurnPhase(trickWinner),
          game_follow_suit,
          // notis: state.notis @ followSuitNotis
        }

        let state = state |> advanceRound

        let (gameOverTestState, (h, l, j, g)) = Util.updateUntil(
          getActiveScoreMods(state),
          ((state, _)) => isGameOverTest(state),
          (state, (None, None, None, None)),
        )

        if isGameOverTest(gameOverTestState) {
          let notis = Noti.broadcast(
            ~msg=RoundSummary({
              noti_maybeTeamHigh: h,
              noti_maybeTeamLow: l,
              noti_maybeTeamJack: j,
              noti_maybeTeamGame: g,
            }),
            ~kind=Confirm,
            (),
          )
          let notiEffects = notis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))
          (
            {
              ...gameOverTestState,
              phase: GameOverPhase(Quad.make(_ => RematchUnknown)),
            },
            Belt.List.concat(notiEffects, effects),
          )
        } else if (
          /* Any player whose hand is empty at this points indicates all players' hands are empty */
          List.length(Quad.get(N1, state.players).pla_hand) == 0
        ) {
          reduceRec(NewRound, state, effects)
        } else {
          (state, effects)
        }
      }
    | Beg =>
      switch state.phase {
      | BegPhase =>
        let beggerId = Quad.nextId(state.dealer)
        switch state.clients->Quad.get(beggerId, _) {
        | Vacant => (state, effects)
        | Detached(client, _)
        | Attached(client) =>
          let notis = Noti.playerBroadcast(
            ~from=beggerId,
            ~msg=Noti.Text(client.client_username ++ " begs"),
            (),
          )
          let notiEffects = notis->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))
          (
            {
              ...state,
              phase: GiveOnePhase,
            },
            Belt.List.concat(notiEffects, effects),
          )
        }
      | _ =>
        logger.warn("`Beg` recieved out of phase.")
        (state, effects)
      }

    | Stand =>
      switch state.phase {
      | BegPhase =>
        let beggerId = Quad.nextId(state.dealer)

        switch Quad.get(beggerId, state.clients) {
        | Vacant => (state, effects)
        | Detached(begger, _)
        | Attached(begger) =>
          let (maybeTeamHigh, maybeTeamLow) = getTeamHighAndLowMaybes(
            state.players->Quad.map(player => player.pla_hand, _),
            state.maybeTrumpCard,
          )

          let notiEffects =
            Noti.playerBroadcast(
              ~from=beggerId,
              ~msg=Noti.Text(begger.client_username ++ " stands"),
              (),
            )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

          let effects = Belt.List.concat(notiEffects, effects)
          let state' = {
            ...state,
            maybeTeamHigh,
            maybeTeamLow,
          }

          (
            {
              ...state',
              phase: PlayerTurnPhase(beggerId),
            },
            effects,
          )
        }
      | _ =>
        logger.warn("`Stand` recieved out of phase.")
        (state, effects)
      }

    // | GiveOne
    //     when
    //       [Quad.N1, N3]
    //       |> List.mem(state.dealer)
    //       && GameTeams.get(T2, state.teams).team_score == 13
    //       || [Quad.N2, N4]
    //       |> List.mem(state.dealer)
    //       && GameTeams.get(T1, state.teams).team_score == 13 =>
    //   state;
    | GiveOne =>
      switch state.phase {
      | GiveOnePhase =>
        switch Quad.get(state.dealer, state.clients) {
        | Vacant => (state, effects)
        | Detached(dealer, _)
        | Attached(dealer) =>
          let receivingTeamId = switch state.dealer {
          | N1 | N3 => Team.T2
          | N2 | N4 => T1
          }

          let (maybeTeamHigh, maybeTeamLow) = getTeamHighAndLowMaybes(
            state.players->Quad.map(player => player.pla_hand, _),
            state.maybeTrumpCard,
          )

          let giveOneNotiEffects =
            Noti.playerBroadcast(
              ~from=state.dealer,
              ~msg=Noti.Text(dealer.client_username ++ " gives one."),
              (),
            )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

          let state' = {
            ...state,
            teams: GameTeams.update(
              receivingTeamId,
              x => {...x, team_score: x.team_score + 1},
              state.teams,
            ),
            maybeTeamHigh,
            maybeTeamLow,
          }

          (
            {
              ...state',
              phase: isGameOverTest(state')
                ? GameOverPhase(Quad.make(_ => RematchUnknown))
                : PlayerTurnPhase(state.leader),
            },
            Belt.List.concat(giveOneNotiEffects, effects),
          )
        }
      | _ =>
        logger.warn("`GiveOne` recieved out of phase.")
        (state, effects)
      }

    | RunPack =>
      switch state.phase {
      | GiveOnePhase
      | RunPackPhase =>
        let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, state.deck)
        let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck)
        let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck)
        let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToRun, deck)

        let client_username = state.clients->Game.getUsername(state.dealer)

        let {Card.suit: prevTrumpSuit} as prevTrumpCard = switch state.maybeTrumpCard {
        | None =>
          failwith("RunPack action expected state.maybeTrumpCard to be Some thing but got None")
        | Some(k) => k
        }

        let (cards, deck) = Deck.deal(1, deck)
        let {Card.rank: nextTrumpRank, Card.suit: nextTrumpSuit} as nextTrumpCard = List.hd(cards)
        let pointsKicked = kickPoints(nextTrumpRank)

        let nextPhase =
          prevTrumpSuit == nextTrumpSuit
            ? List.length(deck) < SharedGame.settings.nCardsToRun * 4 + 1 // enough for 4 players and kicking 1 trump?
                ? FlipFinalTrumpPhase
                : RunPackPhase
            : PlayerTurnPhase(state.leader)

        let runPackNotiEffects =
          Noti.playerBroadcast(
            ~from=state.dealer,
            ~msg=Noti.Text(client_username ++ " runs the pack"),
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

        let nextPlayers =
          state.players
          ->Quad.zip((p1Hand, p2Hand, p3Hand, p4Hand))
          ->Quad.map(((p, hand)) => {...p, pla_hand: Belt.List.concat(p.pla_hand, hand)}, _)

        let nextTrumpCardMaybe = Some(nextTrumpCard)
        let (maybeTeamHigh, maybeTeamLow) = getTeamHighAndLowMaybes(
          nextPlayers->Quad.map(player => player.pla_hand, _),
          nextTrumpCardMaybe,
        )

        let kickTrumpNotiEffects =
          getKickTrumpNotis(nextTrumpCardMaybe)->Belt.List.map(noti => ServerEvent.NotifyPlayer(
            game_key,
            noti,
          ))

        let nextState = {
          ...state,
          phase: nextPhase,
          players: nextPlayers,
          deck: Belt.List.concat(deck, list{prevTrumpCard}),
          maybeTrumpCard: nextTrumpCardMaybe,
          teams: GameTeams.update(
            teamOfPlayer(state.dealer),
            x => {...x, team_score: x.team_score + pointsKicked},
            state.teams,
          ),
          maybeTeamHigh,
          maybeTeamLow,
        }

        (
          {
            ...nextState,
            phase: isGameOverTest(nextState)
              ? GameOverPhase(Quad.make(_ => RematchUnknown))
              : nextState.phase,
          },
          Belt.List.concat(kickTrumpNotiEffects, Belt.List.concat(runPackNotiEffects, effects)),
        )

      | _ =>
        logger.warn("`RunPack` recieved out of phase.")
        (state, effects)
      }

    | DealAgain =>
      switch state.phase {
      | PackDepletedPhase =>
        let client_username = state.clients->getUsername(state.dealer)
        let redealNotiEffects =
          Noti.playerBroadcast(
            ~from=state.dealer,
            ~msg=Noti.Text(client_username ++ " has to redeal"),
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))
        (
          {
            ...state,
            players: Quad.map(x => {...x, pla_hand: list{}}, state.players),
            maybeTrumpCard: None,
            deck: Deck.make() |> Deck.shuffle,
            phase: DealPhase,
          },
          Belt.List.concat(redealNotiEffects, effects),
        )
      | _ =>
        logger.warn("`DealAgain` recieved out of phase.")
        (state, effects)
      }

    | LeaveGame(leavingPlayerId) =>
      switch state.clients->Quad.get(leavingPlayerId, _) {
      | Vacant
      | Detached(_) =>
        logger.warn("Ignoring `LeaveGame` recieved for headless player.")
        (state, effects)
      | Attached(client) =>
        let getNextPhase = (nPlayersToFind, currentPhase) =>
          if isNewGameCheck(state) {
            FindPlayersPhase({emptySeatCount: nPlayersToFind, canSub: false})
          } else {
            switch currentPhase {
            | FindSubsPhase({phase: subPhase}) =>
              FindSubsPhase({emptySeatCount: nPlayersToFind, phase: subPhase})
            | FindPlayersPhase({canSub}) =>
              FindPlayersPhase({emptySeatCount: nPlayersToFind, canSub})
            | GameOverPhase(rematchDecisions) =>
              let rematchDecisions = rematchDecisions->Quad.put(leavingPlayerId, RematchDenied, _)
              let numRematchUnknowns =
                rematchDecisions->Quad.countHaving(_, decision => decision == RematchUnknown)
              if numRematchUnknowns == 0 {
                FindPlayersPhase({emptySeatCount: nPlayersToFind, canSub: false})
              } else {
                GameOverPhase(rematchDecisions)
              }

            | IdlePhase(reason) =>
              FindSubsPhase({emptySeatCount: nPlayersToFind, phase: IdlePhase(reason)})
            | phase => FindSubsPhase({emptySeatCount: nPlayersToFind, phase})
            }
          }

        let nextClients =
          state.clients->Quad.put(
            leavingPlayerId,
            Detached(client, {client_detached_at: Js.Date.now()}),
            _,
          )

        let playerLeftNotiEffects =
          Noti.playerBroadcast(
            ~from=leavingPlayerId,
            ~msg=Noti.Text(client.client_username ++ " has left the game."),
            ~level=Warning,
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

        /*
         * When the leaving player was the game master and the game has at least
         * one other seat still taken, choose a new game master from one of the
         * taken seats.
         */
        let nextGameId = switch state.game_id {
        | Private({private_game_key, private_game_host}) if private_game_host == leavingPlayerId =>
          switch nextClients
          ->Quad.withId
          ->Quad.find(((_quadId, client)) => client->Game.isClientAttached) {
          /* None means no taken seats were found => no players in the game => this game will be discarded by the ServerStore
           So I just leave the id as it is. */
          | None => state.game_id
          | Some((idOfTakenSeat, _client)) =>
            Private({private_game_key, private_game_host: idOfTakenSeat})
          }
        | game_id => game_id
        }

        let getIsTransitionToRematch = (prevPhase, nextPhase) =>
          switch prevPhase {
          | GameOverPhase(_) =>
            switch nextPhase {
            | FindPlayersPhase(_) => true
            | _ => false
            }
          | _ => false
          }

        let nextPhase = getNextPhase(
          nextClients->Quad.countHaving(clientState => !Game.isClientAttached(clientState)),
          state.phase,
        )

        let nextState = if getIsTransitionToRematch(state.phase, nextPhase) {
          initializeRematch(
            nextGameId,
            nextClients,
            nextPhase,
            getRematchDealerId(state.dealer, state.teams),
          )
        } else {
          {
            ...state,
            game_id: nextGameId,
            clients: nextClients,
            phase: nextPhase,
          }
        }

        (nextState, Belt.List.concat(playerLeftNotiEffects, effects))
      }

    | UpdateSubbing(canSub) =>
      let phase = switch state.phase {
      | FindPlayersPhase({emptySeatCount}) => FindPlayersPhase({emptySeatCount, canSub})
      | phase => phase
      }
      ({...state, phase}, effects)
    | StartGame => ({...state, phase: DealPhase}, effects)
    | PrivateToPublic =>
      let state = switch state.game_id {
      | Public(_) => state
      | Private({private_game_key: key}) => {...state, game_id: Public(key)}
      }
      (state, effects)
    | Transition({fromPhase, toPhase}) => (
        {
          ...state,
          phase: state.phase == fromPhase ? toPhase : state.phase,
        },
        effects,
      )

    | AttachClient(seat_id, clientState) =>
      switch clientState {
      | Vacant => (state, effects)
      | Attached({client_username})
      | Detached({client_username}, _) =>
        let clients = state.clients->Quad.put(seat_id, clientState, _)
        let playersNeeded =
          clients->Quad.countHaving(clientState => !Game.isClientAttached(clientState))
        let phase = switch state.phase {
        | FindSubsPhase({phase: subPhase}) =>
          FindSubsPhase({emptySeatCount: playersNeeded, phase: subPhase})
        | FindPlayersPhase({canSub}) => FindPlayersPhase({emptySeatCount: playersNeeded, canSub})
        | otherPhase => otherPhase
        }

        let playerJoinedNotiEffects =
          Noti.playerBroadcast(
            ~from=seat_id,
            ~msg=Noti.Text(client_username ++ " joined the game."),
            ~level=Noti.Success,
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

        let effectsNext = switch phase {
        | FindPlayersPhase({emptySeatCount: 0}) =>
          list{
            ServerEvent.CreateGameTimer(game_key, TransitionGameCountdown(phase, DealPhase)),
            ...playerJoinedNotiEffects,
          }
        | FindSubsPhase({emptySeatCount: 0, phase: subPhase}) =>
          list{
            ServerEvent.CreateGameTimer(game_key, TransitionGameCountdown(phase, subPhase)),
            ...playerJoinedNotiEffects,
          }
        | _ => playerJoinedNotiEffects
        }

        ({...state, clients, phase}, Belt.List.concat(effects, effectsNext))
      }

    | PlayerRematch(seat_id) =>
      let getNextPhase = rematchDecisions => {
        let isRematchPrimed = rematchDecisions->SharedGame.isRematchPrimed
        let numRematchDenied = rematchDecisions->SharedGame.countRematchDenied

        switch (isRematchPrimed, numRematchDenied) {
        | (true, numRematchDenied) if numRematchDenied > 0 =>
          FindPlayersPhase({emptySeatCount: numRematchDenied, canSub: false})
        | _ => GameOverPhase(rematchDecisions)
        }
      }

      let getNextState = (state, seat_id) =>
        switch state.phase {
        | GameOverPhase(rematchDecisions) =>
          let nextRematchDecisions =
            rematchDecisions->Quad.put(seat_id, SharedGame.RematchAccepted, _)
          let nextPhase = getNextPhase(nextRematchDecisions)
          let nextDealerId = getRematchDealerId(state.dealer, state.teams)
          switch nextPhase {
          | FindPlayersPhase(_) =>
            initializeRematch(state.game_id, state.clients, nextPhase, nextDealerId)
          | GameOverPhase(rematchDecisions)
            if SharedGame.isRematchAcceptedByAll(rematchDecisions) =>
            initializeRematch(state.game_id, state.clients, nextPhase, nextDealerId)
          | _ => {...state, phase: nextPhase}
          }
        | _ => state
        }

      (getNextState(state, seat_id), effects)

    | StartRematch =>
      switch state.phase {
      | GameOverPhase(rematchDecisions) if isRematchPrimed(rematchDecisions) =>
        // When all players have chosen to rematch or leave, reinit the game with the rematching players.
        // This may mean that the game goes into the FindPlayersPhase if some players left the game instead of
        // rematching. Or it may go into the deal phase if all players chose to rematch
        let numRematchingPlayers =
          rematchDecisions->Quad.countHaving(decision => decision == RematchAccepted)

        let phase =
          numRematchingPlayers == 4
            ? Game.DealPhase
            : FindPlayersPhase({emptySeatCount: 4 - numRematchingPlayers, canSub: false})

        (
          {
            ...state,
            phase,
          },
          effects,
        )

      | _ => (state, effects)
      }

    | FlipAgain =>
      switch state.phase {
      | FlipFinalTrumpPhase =>
        let client_username = state.clients->Game.getUsername(state.dealer)

        let {Card.suit: prevTrumpSuit} as prevTrumpCard = state->Game.getTrumpCardExn
        let (card, deck) = Deck.deal1Exn(state.deck)
        let {Card.rank: nextTrumpRank, Card.suit: nextTrumpSuit} as nextTrumpCard = card
        let pointsKicked = kickPoints(nextTrumpRank)

        let nextPhase =
          prevTrumpSuit == nextTrumpSuit ? PackDepletedPhase : PlayerTurnPhase(state.leader)
        let nextTrumpCardMaybe = Some(nextTrumpCard)

        let (maybeTeamHigh, maybeTeamLow) = getTeamHighAndLowMaybes(
          state.players->Quad.map(player => player.pla_hand, _),
          nextTrumpCardMaybe,
        )

        let kickTrumpNotiEffects =
          getKickTrumpNotis(nextTrumpCardMaybe)->Belt.List.map(noti => ServerEvent.NotifyPlayer(
            game_key,
            noti,
          ))

        let runPackNotiEffects =
          Noti.playerBroadcast(
            ~from=state.dealer,
            ~msg=Noti.Text(client_username ++ " flips the last card."),
            (),
          )->Belt.List.map(noti => ServerEvent.NotifyPlayer(game_key, noti))

        let nextState = {
          ...state,
          phase: nextPhase,
          deck: Belt.List.concat(deck, list{prevTrumpCard}),
          maybeTrumpCard: nextTrumpCardMaybe,
          teams: GameTeams.update(
            teamOfPlayer(state.dealer),
            x => {...x, team_score: x.team_score + pointsKicked},
            state.teams,
          ),
          maybeTeamHigh,
          maybeTeamLow,
        }

        (
          {
            ...nextState,
            phase: isGameOverTest(nextState)
              ? GameOverPhase(Quad.make(_ => RematchUnknown))
              : nextState.phase,
          },
          Belt.List.concat(kickTrumpNotiEffects, Belt.List.concat(runPackNotiEffects, effects)),
        )
      | _ => (state, effects)
      }
    }

  reduceRec(action, state, list{}) |> addLeaveStateEffects(state) |> addEnterStateEffects(state)
}