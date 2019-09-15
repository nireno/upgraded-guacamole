open AppPrelude;
include SharedGame;

type playerData = {
  pla_hand: Hand.FaceUpHand.t,
  pla_tricks: list(Trick.t),
  pla_card: option(Card.t), /* Card on board */
};

type clientData = {
  client_socket_id: sock_id,
  client_username: string,
  client_id: string,
  client_initials: string,
  client_profile_type: ClientSettings.profileType,
  client_connected_at: milliseconds,
};

type clientMetaData = {
  client_disconnected_at: milliseconds,
};

type clientState = 
| Connected(clientData)
| Disconnected(clientData, clientMetaData)
| Vacant;

let getUsername = (clients, quadId) =>
  switch (clients->Quad.get(quadId, _)) {
  | Connected(client)
  | Disconnected(client, _) => client.client_username
  | Vacant => Player.stringOfId(quadId)
  };

let countConnectedClients = clients => {
  let clientIsConnected =
    fun
    | Connected(_) => true
    | _ => false;

  clients->Quad.countHaving(clientIsConnected);
};

let initPlayerData = () => {
  pla_hand: [],
  pla_tricks: [],
  pla_card: None,
};

[@decco] type notis = list(Noti.t);

type findPlayersContext = { emptySeatCount: int, canSub: bool };

type phase =
  | IdlePhase(option(Timer.timeout), idleReason)
  | FindSubsPhase(findSubsContext)
  | FindPlayersPhase(findPlayersContext)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase(Quad.t(rematchDecision))
and findSubsContext = { emptySeatCount: int, phase}
and idlePhaseContext = { maybeTimer: option(Timer.timeout), fromPhase: phase, toPhase: phase};

let rec stringOfPhase =
  fun
  | IdlePhase(_) => "IdlePhase"
  | FindSubsPhase({emptySeatCount, phase}) =>
    "FindSubsPhase(" ++ string_of_int(emptySeatCount) ++ ", " ++ stringOfPhase(phase) ++ ")"
  | FindPlayersPhase({emptySeatCount, canSub}) =>
    "FindPlayersPhase(" ++ string_of_int(emptySeatCount) ++ ", " ++ string_of_bool(canSub) ++ ")"
  | DealPhase => "DealPhase"
  | BegPhase => "BegPhase"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | PlayerTurnPhase(playerId) => "PlayerTurnPhase(" ++ Quad.stringifyId(playerId) ++ ")"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase(_) => "GameOverPhase";

let isPlayerActivePhase = fun
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(_) 
  | PackDepletedPhase => true
  | IdlePhase(_)
  | FindSubsPhase(_)
  | FindPlayersPhase(_)
  | GameOverPhase(_) => false;

let isFaceDownPhase =
  fun
  | FindSubsPhase({ phase: BegPhase })
  | BegPhase
  | FindSubsPhase({ phase: GiveOnePhase })
  | GiveOnePhase => true
  | _ => false;

type state = {
  game_id,
  deck: Deck.t,
  players: (playerData, playerData, playerData, playerData),
  clients: (clientState, clientState, clientState, clientState),
  teams: (teamState, teamState),
  notis,
  maybeTrumpCard: option(Card.t),
  maybeLeadCard: option(Card.t),
  dealer: Player.id,
  leader: Player.id,
  maybeTeamHigh: option(GameAward.luckyAwardData),
  maybeTeamLow: option(GameAward.luckyAwardData),
  maybeTeamJack: option(GameAward.jackAwardData),
  phase,
  maybeKickTimeoutId: option(Js.Global.timeoutId),
  game_follow_suit: option(Card.Suit.t),
};

let isSeatTaken = clientState =>
  switch (clientState) {
  | Vacant => false
  | Connected(_)
  | Disconnected(_, _) => true
  };


module SockServ = BsSocket.Server.Make(SocketMessages);

let debugOfState = (state) => {
  let stringOfPlayer = player => {
    let card = Card.codeOfMaybeCard(player.pla_card);
    let tricks =
      List.map(Trick.codeOfTrick, player.pla_tricks)
      |> Belt.List.toArray
      |> Js.Array.joinWith(", ");

    {j|{card-on-board: $card, tricks-taken: [$tricks] }|j};
  };

  let stringOfClientData = ({client_socket_id, client_username, client_id, client_initials}) => {
    {j|{$client_socket_id, $client_username, $client_id, $client_initials}|j};
  };

  let stringOfClient = client => {
    switch (client) {
    | Vacant => "Vacant"
    | Connected(clientData) =>
      let clientDataText = clientData->stringOfClientData;
      {j|Connected($clientDataText)|j};
    | Disconnected(clientData, _) =>
      let clientDataText = clientData->stringOfClientData;
      {j|Disconnected($clientDataText)|j};
    };
  };

  let stringOfTeamHigh =
    state.maybeTeamHigh
      ->Belt.Option.mapWithDefault("None", GameAward.stringOfLuckyAwardData);

  let stringOfTeamLow = 
    state.maybeTeamLow
     ->Belt.Option.mapWithDefault("None", GameAward.stringOfLuckyAwardData)

  let stringOfTeamJack = 
    state.maybeTeamJack
      ->Belt.Option.mapWithDefault("None", GameAward.stringOfJackAwardData)
  
  let debugOfPlayers = {
    "Player1": Quad.select(N1, stringOfPlayer, state.players),
    "Player2": Quad.select(N2, stringOfPlayer, state.players),
    "Player3": Quad.select(N3, stringOfPlayer, state.players),
    "Player4": Quad.select(N4, stringOfPlayer, state.players),
  };

  let debugOfClients = {
    "Client 1": Quad.select(N1, stringOfClient, state.clients),
    "Client 2": Quad.select(N2, stringOfClient, state.clients),
    "Client 3": Quad.select(N3, stringOfClient, state.clients),
    "Client 4": Quad.select(N4, stringOfClient, state.clients),
  };

  let debugOf_game_follow_suit =
    state.game_follow_suit
    ->Belt.Option.mapWithDefault("None", (cardSuit) =>
        "Some(" ++ cardSuit->Card.Suit.toString ++ ")"
      );

  {
    "game_id": state.game_id,
    "phase": stringOfPhase(state.phase),
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
  };
};

let initialState = () => {
  {
    game_id: Public(""),
    deck: Deck.make() |> Deck.shuffle,
    players: (
      initPlayerData(),
      initPlayerData(),
      initPlayerData(),
      initPlayerData(),
    ),
    clients: (Vacant, Vacant, Vacant, Vacant),
    teams: (initialTeamState, initialTeamState),
    notis: [],
    maybeTrumpCard: None,
    maybeLeadCard: None,
    dealer: N1,
    leader: N2,
    maybeTeamHigh: None,
    maybeTeamLow: None,
    maybeTeamJack: None,
    phase: FindPlayersPhase({ emptySeatCount: 4, canSub: false }),
    maybeKickTimeoutId: None,
    game_follow_suit: None,
  };
};

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
    |> Js.Array.joinWith(" ");

  {...initialState(), game_id: Private({private_game_key: key, private_game_host: Quad.N1})};
};

let findEmptySeat = state => {
  switch (state.clients->Quad.withId->Quad.find(((_, clientState)) => clientState == Vacant)) {
  | None => None
  | Some((seatId, _)) => Some(seatId)
  };
};

let isGameOverTest = state => {
  GameTeams.get(T1, state.teams).team_score >= SharedGame.settings.winningScore
  || GameTeams.get(T2, state.teams).team_score >= SharedGame.settings.winningScore;
};


let trickContainsCard: (Card.t, Trick.t) => bool =
  (testCard, trick) => {
    Quad.toDict(trick)
    |> List.exists(((_, card)) => card == testCard);
  };


let playerOfIntUnsafe =
  fun
  | 1 => Quad.N1
  | 2 => N2
  | 3 => N3
  | 4 => N4
  | n =>
    failwith("Expected a number in [1, 4] but got: " ++ string_of_int(n));


module Filter = {
  type privacy = Private | Public | PrivateOrPublic;
  type simplePhase = FindPlayersPhase | FindSubsPhase | Other;


  let simplifyPhase: phase => simplePhase =
    gamePhase => {
      switch (gamePhase) {
      | FindPlayersPhase(_) => FindPlayersPhase
      | FindSubsPhase(_) => FindSubsPhase
      | _ => Other
      };
    };

  let hasPrivacy = (gameState, privacy) => 
    switch(gameState.game_id){
    | Public(_) when privacy == Public => true
    | Private(_) when privacy == Private => true
    | _ when privacy == PrivateOrPublic => true
    | _ => false
    }

  let hasPhase = ( gameState , simplePhase) => 
    simplifyPhase(gameState.phase) == simplePhase;
}

/* A game is considered empty if no player slot has a socket attached. */
let isEmpty = (state) => {
  let isHeadless = fun
  | Vacant => true
  | Disconnected(_) => true
  | Connected(_) => false;
  state.clients->Quad.every(isHeadless, _)
};

let isPublic = (state) => {
  switch(state.game_id){
  | Public(_) => true
  | Private(_) => false
  }
};

let isPrivate = (state) => {
  switch(state.game_id){
  | Public(_) => false
  | Private(_) => true
  }
};

let isFindPlayersPhase = fun
| FindPlayersPhase(_) => true
| _ => false;

let isFindSubsPhase = fun
| FindSubsPhase(_) => true
| _ => false;


let decidePlayerPhase: (phase, Player.id, Player.id) => (Player.id, Player.phase) =
  (gamePhase, dealerId, playerId) => {

      let playerPhase =
        switch (gamePhase) {
        | PlayerTurnPhase(playerId) => Player.PlayerTurnPhase(playerId)
        | DealPhase when dealerId == playerId => PlayerDealPhase
        | GiveOnePhase when dealerId == playerId => PlayerGiveOnePhase
        | RunPackPhase when dealerId == playerId => PlayerRunPackPhase
        | PackDepletedPhase when dealerId == playerId => PlayerRedealPhase
        | BegPhase when Quad.nextId(dealerId) == playerId => PlayerBegPhase
        | _ => PlayerIdlePhase
        };
      (playerId, playerPhase);
    };

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
