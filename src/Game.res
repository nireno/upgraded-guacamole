open AppPrelude
include SharedGame

let logger = appLogger.makeChild({"_context": "Game.res"})

@decco
type playerData = {
  pla_hand: Hand.FaceUpHand.t,
  pla_tricks: list<Trick.t>,
  pla_card: option<Card.t> /* Card on board */,
}

@decco
type clientData = {
  client_socket_id: sock_id,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
  client_connected_at: milliseconds,
}

@decco
type clientMetaData = {client_detached_at: milliseconds}

@decco
type clientState =
  | Attached(clientData)
  | Detached(clientData, clientMetaData)
  | Vacant

let isClientAttached = clientState =>
  switch clientState {
  | Attached(_) => true
  | Detached(_, _)
  | Vacant => false
  }

let getUsername = (clients, quadId) =>
  switch clients->Quad.get(quadId, _) {
  | Attached(client)
  | Detached(client, _) =>
    client.client_username
  | Vacant => Player.stringOfId(quadId)
  }

let countAttachedClients = clients => {
  let clientIsConnected = x =>
    switch x {
    | Attached(_) => true
    | _ => false
    }

  clients->Quad.countHaving(clientIsConnected)
}

let countConnectedClients = countAttachedClients

let countMissingClients = clients => {
  let clientIsMissing = x =>
    switch x {
    | Detached(_)
    | Vacant => true
    | Attached(_) => false
    }

  clients->Quad.countHaving(clientIsMissing)
}

let initPlayerData = () => {
  pla_hand: list{},
  pla_tricks: list{},
  pla_card: None,
}

@decco type notis = list<Noti.t>

type findPlayersContext = {emptySeatCount: int, canSub: bool}
type begPhaseContext = BegPhaseDeciding | BegPhaseStanding

module Phase = {
  type rec t =
    | IdlePhase(idleReason)
    | FindSubsPhase(findSubsContext)
    | FindPlayersPhase(findPlayersContext)
    | DealPhase
    | BegPhase(begPhaseContext)
    | GiveOnePhase
    | RunPackPhase
    | PlayerTurnPhase(Player.id)
    | PackDepletedPhase
    | FlipFinalTrumpPhase
    | GameOverPhase(Quad.t<rematchDecision>)
  and findSubsContext = {emptySeatCount: int, phase: t}
  and idlePhaseContext = {maybeTimer: option<Timer.timeout>, fromPhase: t, toPhase: t}

  let rec t_encode = phase => {
    switch phase {
    | IdlePhase(idleReason) =>
      Js.Dict.fromArray([
        ("phase", "IdlePhase"->Js.Json.string),
        ("idleReason", idleReason->idleReason_encode),
      ])->Js.Json.object_
    | FindSubsPhase({emptySeatCount, phase}) =>
      Js.Dict.fromArray([
        ("phase", "FindSubsPhase"->Js.Json.string),
        ("emptySeatCount", emptySeatCount->Belt.Int.toFloat->Js.Json.number),
        ("phase", t_encode(phase)),
      ])->Js.Json.object_
    | FindPlayersPhase({emptySeatCount, canSub}) =>
      Js.Dict.fromArray([
        ("phase", "FindPlayersPhase"->Js.Json.string),
        ("emptySeatCount", emptySeatCount->Belt.Int.toFloat->Js.Json.number),
        ("canSub", canSub->Js.Json.boolean),
      ])->Js.Json.object_
    | DealPhase => "DealPhase"->Js.Json.string
    | BegPhase(BegPhaseDeciding) => "BegPhase(BegPhaseDeciding)"->Js.Json.string
    | BegPhase(BegPhaseStanding) => "BegPhase(BegPhaseStanding)"->Js.Json.string
    | GiveOnePhase => "GiveOnePhase"->Js.Json.string
    | RunPackPhase => "RunPackPhase"->Js.Json.string
    | FlipFinalTrumpPhase => "FlipFinalTrumpPhase"->Js.Json.string
    | PlayerTurnPhase(playerId) =>
      Js.Dict.fromArray([
        ("phase", "PlayerTurnPhase"->Js.Json.string),
        ("playerId", Quad.stringifyId(playerId)->Js.Json.string),
      ])->Js.Json.object_
    | PackDepletedPhase => "PackDepletedPhase"->Js.Json.string
    | GameOverPhase(_) => "GameOverPhase"->Js.Json.string
    }
  }

  /** Not yet implemented */
  let t_decode = _json => {
    logger.error("Phase.t_decode not yet implemented")->Obj.magic
  }

  let rec toString = x =>
    switch x {
    | IdlePhase(_) => "IdlePhase"
    | FindSubsPhase({emptySeatCount, phase}) =>
      "FindSubsPhase(" ++ (string_of_int(emptySeatCount) ++ (", " ++ (toString(phase) ++ ")")))
    | FindPlayersPhase({emptySeatCount, canSub}) =>
      "FindPlayersPhase(" ++
      (string_of_int(emptySeatCount) ++
      (", " ++ (string_of_bool(canSub) ++ ")")))
    | DealPhase => "DealPhase"
    | BegPhase(BegPhaseDeciding) => "BegPhase(BegPhaseDeciding)"
    | BegPhase(BegPhaseStanding) => "BegPhase(BegPhaseStanding)"
    | GiveOnePhase => "GiveOnePhase"
    | RunPackPhase => "RunPackPhase"
    | FlipFinalTrumpPhase => "FlipFinalTrumpPhase"
    | PlayerTurnPhase(playerId) => "PlayerTurnPhase(" ++ (Quad.stringifyId(playerId) ++ ")")
    | PackDepletedPhase => "PackDepletedPhase"
    | GameOverPhase(_) => "GameOverPhase"
    }
}

let isPlayerActivePhase = (x: Phase.t) =>
  switch x {
  | DealPhase
  | BegPhase(BegPhaseDeciding)
  | BegPhase(BegPhaseStanding)
  | GiveOnePhase
  | RunPackPhase
  | FlipFinalTrumpPhase
  | PlayerTurnPhase(_)
  | PackDepletedPhase => true
  | IdlePhase(_)
  | FindSubsPhase(_)
  | FindPlayersPhase(_)
  | GameOverPhase(_) => false
  }

let isFaceDownPhase = (x: Phase.t) =>
  switch x {
  | FindSubsPhase({phase: BegPhase(_)})
  | BegPhase(_)
  | FindSubsPhase({phase: GiveOnePhase})
  | GiveOnePhase => true
  | _ => false
  }

@decco
type state = {
  game_id: game_id,
  deck: Deck.t,
  players: (playerData, playerData, playerData, playerData),
  clients: (clientState, clientState, clientState, clientState),
  teams: (teamState, teamState),
  maybeTrumpCard: option<Card.t>,
  maybeLeadCard: option<Card.t>,
  dealer: Player.id,
  leader: Player.id,
  maybeTeamHigh: option<GameAward.luckyAwardData>,
  maybeTeamLow: option<GameAward.luckyAwardData>,
  maybeTeamJack: option<GameAward.jackAwardData>,
  phase: Phase.t,
  game_follow_suit: option<Card.Suit.t>,
}

module SockServ = BsSocketio.Server.Make(SocketMessages)

let debugOfState = state => {
  let stringOfPlayer = player => {
    let card = Card.codeOfMaybeCard(player.pla_card)
    let tricks =
      List.map(Trick.codeOfTrick, player.pla_tricks) |> Belt.List.toArray |> Js.Array.joinWith(", ")

    j`{card-on-board: $card, tricks-taken: [$tricks] }`
  }

  let stringOfClientData = ({client_socket_id, client_username, client_id, client_initials}) =>
    j`{$client_socket_id, $client_username, $client_id, $client_initials}`

  let stringOfClient = client =>
    switch client {
    | Vacant => "Vacant"
    | Attached(clientData) =>
      let clientDataText = clientData->stringOfClientData
      j`Attached($clientDataText)`
    | Detached(clientData, _) =>
      let clientDataText = clientData->stringOfClientData
      j`Detached($clientDataText)`
    }

  let stringOfTeamHigh =
    state.maybeTeamHigh->Belt.Option.mapWithDefault("None", GameAward.stringOfLuckyAwardData)

  let stringOfTeamLow =
    state.maybeTeamLow->Belt.Option.mapWithDefault("None", GameAward.stringOfLuckyAwardData)

  let stringOfTeamJack =
    state.maybeTeamJack->Belt.Option.mapWithDefault("None", GameAward.stringOfJackAwardData)

  let debugOfPlayers = {
    "Player1": Quad.select(N1, stringOfPlayer, state.players),
    "Player2": Quad.select(N2, stringOfPlayer, state.players),
    "Player3": Quad.select(N3, stringOfPlayer, state.players),
    "Player4": Quad.select(N4, stringOfPlayer, state.players),
  }

  let debugOfClients = {
    "Client 1": Quad.select(N1, stringOfClient, state.clients),
    "Client 2": Quad.select(N2, stringOfClient, state.clients),
    "Client 3": Quad.select(N3, stringOfClient, state.clients),
    "Client 4": Quad.select(N4, stringOfClient, state.clients),
  }

  let debugOf_game_follow_suit =
    state.game_follow_suit->Belt.Option.mapWithDefault("None", cardSuit =>
      "Some(" ++ (cardSuit->Card.Suit.toString ++ ")")
    )

  {
    "game_id": state.game_id,
    "phase": Phase.toString(state.phase),
    "dealer": Player.stringOfId(state.dealer),
    "leader": Player.stringOfId(state.leader),
    "maybeTrumpCard": Card.codeOfMaybeCard(state.maybeTrumpCard),
    "maybeLeadCard": Card.codeOfMaybeCard(state.maybeLeadCard),
    "maybeTeamHigh": stringOfTeamHigh,
    "maybeTeamLow": stringOfTeamLow,
    "maybeTeamJack": stringOfTeamJack,
    "players": debugOfPlayers,
    "clients": debugOfClients,
    "game_follow_suit": debugOf_game_follow_suit,
  }
}

let initialState = () => {
  game_id: Public(""),
  deck: Deck.make() |> Deck.shuffle,
  players: (initPlayerData(), initPlayerData(), initPlayerData(), initPlayerData()),
  clients: (Vacant, Vacant, Vacant, Vacant),
  teams: (initialTeamState, initialTeamState),
  maybeTrumpCard: None,
  maybeLeadCard: None,
  dealer: N1,
  leader: N2,
  maybeTeamHigh: None,
  maybeTeamLow: None,
  maybeTeamJack: None,
  phase: FindPlayersPhase({emptySeatCount: 4, canSub: false}),
  game_follow_suit: None,
}

let initPrivateGame = () => {
  // Generate a random string based on the string representation of
  // cards selected from the deck
  let key =
    Deck.make()
    |> Deck.shuffle
    |> Belt.List.take(_, 4)
    |> Js.Option.getExn
    |> List.map(Card.codeOfCard)
    |> Belt.List.toArray
    |> Js.Array.joinWith(" ")

  {...initialState(), game_id: Private({private_game_key: key, private_game_host: Quad.N1})}
}

let findEmptySeat = state =>
  switch state.clients->Quad.withId->Quad.find(((_, clientState)) => clientState == Vacant) {
  | None => None
  | Some((seatId, _)) => Some(seatId)
  }

let isGameOverTest = state =>
  GameTeams.get(T1, state.teams).team_score >= SharedGame.settings.winningScore ||
    GameTeams.get(T2, state.teams).team_score >= SharedGame.settings.winningScore

let trickContainsCard: (Card.t, Trick.t) => bool = (testCard, trick) =>
  Quad.toDict(trick) |> List.exists(((_, card)) => card == testCard)

let playerOfIntUnsafe = x =>
  switch x {
  | 1 => Quad.N1
  | 2 => N2
  | 3 => N3
  | 4 => N4
  | n => failwith("Expected a number in [1, 4] but got: " ++ string_of_int(n))
  }

module Filter = {
  type privacy = Private | Public | PrivateOrPublic
  type simplePhase = FindPlayersPhase | FindSubsPhase | Other

  let simplifyPhase: Phase.t => simplePhase = gamePhase =>
    switch gamePhase {
    | FindPlayersPhase(_) => FindPlayersPhase
    | FindSubsPhase(_) => FindSubsPhase
    | _ => Other
    }

  let hasPrivacy = (gameState, privacy) =>
    switch gameState.game_id {
    | Public(_) if privacy == Public => true
    | Private(_) if privacy == Private => true
    | _ if privacy == PrivateOrPublic => true
    | _ => false
    }

  let hasPhase = (gameState, simplePhase) => simplifyPhase(gameState.phase) == simplePhase
}

/* A game is considered empty if no player slot has a socket attached. */
let isEmpty = state => {
  let isHeadless = x =>
    switch x {
    | Attached(_) => false
    | Detached(_, _)
    | Vacant => true
    }
  state.clients->Quad.every(isHeadless, _)
}

let isPublic = state =>
  switch state.game_id {
  | Public(_) => true
  | Private(_) => false
  }

let isPrivate = state =>
  switch state.game_id {
  | Public(_) => false
  | Private(_) => true
  }

let isFindPlayersPhase = (x: Phase.t) =>
  switch x {
  | FindPlayersPhase(_) => true
  | _ => false
  }

let isFindSubsPhase = (x: Phase.t) =>
  switch x {
  | FindSubsPhase(_) => true
  | _ => false
  }

let decidePlayerPhase: (Phase.t, Player.id, Player.id) => (Player.id, Player.phase) = (
  gamePhase,
  dealerId,
  playerId,
) => {
  let playerPhase = switch gamePhase {
  | PlayerTurnPhase(playerId) => Player.PlayerTurnPhase(playerId)
  | DealPhase if dealerId == playerId => PlayerDealPhase
  | GiveOnePhase if dealerId == playerId => PlayerGiveOnePhase
  | RunPackPhase if dealerId == playerId => PlayerRunPackPhase
  | FlipFinalTrumpPhase if dealerId == playerId => PlayerFlipFinalTrumpPhase
  | PackDepletedPhase if dealerId == playerId => PlayerRedealPhase
  | BegPhase(BegPhaseDeciding) if Quad.nextId(dealerId) == playerId =>
    PlayerBegPhase(PlayerBegPhaseDeciding)
  | BegPhase(BegPhaseStanding) if Quad.nextId(dealerId) == playerId =>
    PlayerBegPhase(PlayerBegPhaseStanding)
  | _ => PlayerIdlePhase
  }
  (playerId, playerPhase)
}

let isNewGameCheck = state => {
  let areScoresZero = switch state.teams {
  | ({team_score: 0}, {team_score: 0}) => true
  | _ => false
  }

  let isDealPhase = switch state.phase {
  | DealPhase => true
  | _ => false
  }
  isDealPhase && areScoresZero
}

let needsSubstitutes = state =>
  switch state.phase {
  | FindSubsPhase({emptySeatCount}) if emptySeatCount > 0 => true
  | _ => false
  }

let getTrumpCardExn = state =>
  switch state.maybeTrumpCard {
  | None => failwith("RunPack action expected state.maybeTrumpCard to be Some thing but got None")
  | Some(k) => k
  }

module Action = {
  type rec t =
    | Noop
    | PlayCard(Player.id, Card.t)
    | AdvanceRound
    | NewRound
    | Beg
    | Stand
    | GiveOne
    | Deal
    | RunPack
    | DealAgain
    | LeaveGame(Player.id)
    | UpdateSubbing(bool)
    | StartGame
    | PrivateToPublic
    | Transition(transitionContext)
    | AttachClient(Quad.id, clientState)
    | PlayerRematch(Player.id)
    | StartRematch
    | FlipAgain
  and transitionContext = {
    fromPhase: Phase.t,
    toPhase: Phase.t,
  }

  let t_encode = event => {
    switch event {
    | Noop => "Noop"->Js.Json.string
    | PlayCard(playerId, card) =>
      Js.Dict.fromArray([
        ("eventName", "PlayCard"->Js.Json.string),
        ("playerId", Player.stringOfId(playerId)->Js.Json.string),
        ("card", Card.codeOfCard(card)->Js.Json.string),
      ])->Js.Json.object_
    | AdvanceRound => "AdvanceRound"->Js.Json.string

    | NewRound => "NewRound"->Js.Json.string
    | Beg => "Beg"->Js.Json.string
    | Stand => "Stand"->Js.Json.string
    | GiveOne => "GiveOne"->Js.Json.string
    | Deal => "Deal"->Js.Json.string
    | RunPack => "RunPack"->Js.Json.string
    | DealAgain => "DealAgain"->Js.Json.string
    | LeaveGame(playerId) =>
      Js.Dict.fromArray([
        ("eventName", "LeaveGame"->Js.Json.string),
        ("playerId", Player.stringOfId(playerId)->Js.Json.string),
      ])->Js.Json.object_
    | UpdateSubbing(isSubbing) =>
      Js.Dict.fromArray([
        ("eventName", "UpdateSubbing"->Js.Json.string),
        ("isSubbing", isSubbing->Js.Json.boolean),
      ])->Js.Json.object_
    | StartGame => "StartGame"->Js.Json.string
    | PrivateToPublic => "PrivateToPublic"->Js.Json.string
    | Transition({fromPhase, toPhase}) =>
      Js.Dict.fromArray([
        ("eventName", "Transition"->Js.Json.string),
        ("fromPhase", Phase.toString(fromPhase)->Js.Json.string),
        ("toPhase", Phase.toString(toPhase)->Js.Json.string),
      ])->Js.Json.object_
    | AttachClient(quadId, clientState) =>
      Js.Dict.fromArray([
        ("eventName", "AttachClient"->Js.Json.string),
        ("quadId", Quad.stringifyId(quadId)->Js.Json.string),
        ("clientState", clientState->clientState_encode),
      ])->Js.Json.object_
    | PlayerRematch(playerId) =>
      Js.Dict.fromArray([
        ("eventName", "PlayerRematch"->Js.Json.string),
        ("playerId", Player.stringOfId(playerId)->Js.Json.string),
      ])->Js.Json.object_
    | StartRematch => "StartRematch"->Js.Json.string
    | FlipAgain => "FlipAgain"->Js.Json.string
    }
  }
}

// module TestState = {
//   // State initializers for testing specific functionality.

//   let initHangJackGame = () => {
//     let aceHearts = {Card.suit: Card.Suit.Hearts, Card.rank: Card.Rank.Ace};
//     let twoHearts = {Card.suit: Card.Suit.Hearts, Card.rank: Card.Rank.Two};
//     let aceOfHearts = {rank: Ace, suit: Hearts};
//     let jackOfHearts = {rank: Jack, suit: Hearts};

//     let maybeTeamHigh = {
//       team_id: T2,
//       winning_card: aceOfHearts,
//       losing_card: jackOfHearts,
//     };

//     let maybeTeamLow = {
//       team_id: T1,
//     }
//     {
//       ...initialState(),
//       game_id: Public("1"),
//       phase: FindSubsPhase(4, PlayerTurnPhase(N2)),
//       players: (
//         {
//           ...initialPlayerState(N1),
//           pla_hand: [{Card.suit: Card.Suit.Hearts, Card.rank: Card.Rank.Jack}, {suit: Clubs, rank: Two}],
//         },
//         {...initialPlayerState(N2), pla_hand: [aceHearts, {suit: Clubs, rank: Three}]},
//         {...initialPlayerState(N3), pla_hand: [twoHearts, {suit: Clubs, rank: Four}]},
//         {
//           ...initialPlayerState(N4),
//           pla_hand: [{Card.suit: Card.Suit.Hearts, Card.rank: Card.Rank.Three}, {suit: Clubs, rank: Five}],
//         },
//       ),
//       maybeTrumpCard: Some({suit: Card.Suit.Hearts, rank: Card.Rank.Ten}),
//       maybeTeamHigh: Some(LuckyPoint(T2, {rank: Ace, suit: Hearts}, Some({rank: Jack, suit: Hearts}))),
//       maybeTeamLow: Some(LuckyPoint(T1, {rank: Two, suit: Hearts}, Some({rank: Three, suit: Hearts}))),
//     };
//   };
// };
