open AppPrelude;
include SharedGame;

type playerState = {
  sock_id_maybe: option(sock_id),
  pla_name: string,
  pla_hand: Hand.FaceUpHand.t,
  pla_tricks: list(Trick.t),
  pla_card: option(Card.t), /* Card on board */
}

let initialPlayerState = playerId => {
  sock_id_maybe: None,
  pla_name: Player.stringOfId(playerId),
  pla_hand: [],
  pla_tricks: [],
  pla_card: None,
};

[@decco] type notis = list(Noti.t);

type state = {
  game_id,
  deck: Deck.t,
  players: (playerState, playerState, playerState, playerState),
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

module SockServ = BsSocket.Server.Make(SocketMessages);

let debugOfState = (state) => {
  let stringOfPlayer = player => {
    let name = player.pla_name;
    let socket = Belt.Option.getWithDefault(player.sock_id_maybe, "None");
    let card = Card.codeOfMaybeCard(player.pla_card);
    let tricks =
      List.map(Trick.codeOfTrick, player.pla_tricks)
      |> Belt.List.toArray
      |> Js.Array.joinWith(", ");

    {j|{$name, $socket, $card, [$tricks] }|j};
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
    "game_follow_suit": debugOf_game_follow_suit,
  };
};

let initialState = () => {
  {
    game_id: Public(""),
    deck: Deck.make() |> Deck.shuffle,
    players: (
      initialPlayerState(N1),
      initialPlayerState(N2),
      initialPlayerState(N3),
      initialPlayerState(N4),
    ),
    teams: (initialTeamState, initialTeamState),
    notis: [],
    maybeTrumpCard: None,
    maybeLeadCard: None,
    dealer: N1,
    leader: N2,
    maybeTeamHigh: None,
    maybeTeamLow: None,
    maybeTeamJack: None,
    phase: FindPlayersPhase(4, false),
    maybeKickTimeoutId: None,
    game_follow_suit: None,
  };
};

let initPrivateGame = () => {
  // Generate a random string based on the string representation of
  // cards selected from the deck
  let strId =
    Deck.make()
    |> Deck.shuffle
    |> Belt.List.take(_, 4)
    |> Js.Option.getExn
    |> List.map(Card.codeOfCard)
    |> Belt.List.toArray
    |> Js.Array.joinWith(" ");

  {...initialState(), game_id: Private(strId)};
};

let getAllPlayerSockets = state => {
  let rec f:
    (Player.id, list((Player.id, sock_id))) =>
    list((Player.id, sock_id)) =
    (player, playerSockets) => {
      let playerSockets =
        switch (Quad.get(player, state.players).sock_id_maybe) {
        | None => playerSockets
        | Some(socket) => [(player, socket), ...playerSockets]
        };
      player == N4 ? playerSockets : f(Quad.nextId(player), playerSockets);
    };
  f(N1, []);
};

let playerCount = state => {
  let (n1, n2, n3, n4) =
    Quad.map(x => Js.Option.isSome(x.sock_id_maybe) ? 1 : 0, state.players);
  n1 + n2 + n3 + n4;
};

let countPlayers = players => {
  let (n1, n2, n3, n4) = 
    Quad.map(x => Js.Option.isSome(x.sock_id_maybe) ? 1 : 0, players);
  n1 + n2 + n3 + n4;
};

let findEmptySeat = state => {
  switch(Quad.toDict(state.players) |> List.filter(((_k:Player.id, v:playerState)) => Js.Option.isNone(v.sock_id_maybe))){
    | [] => None
    | [(pla_id, _v), ..._rest] => Some(pla_id)
  }
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


let hasSocket = ( socket, state ) => {
  getAllPlayerSockets(state)
  |> List.map(((_player, socket)) => socket)
  |> List.mem(socket)
}

module Filter = {
  type privacy = Private | Public | PrivateOrPublic;
  type phase = FindPlayersPhase | FindSubsPhase | Other;


  let simplifyPhase: SharedGame.phase => phase =
    gamePhase => {
      switch (gamePhase) {
      | FindPlayersPhase(_, _) => FindPlayersPhase
      | FindSubsPhase(_, _) => FindSubsPhase
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

let maybeGetSocketPlayer = (socket, state) => {
  getAllPlayerSockets(state)
  |> List.fold_left(
       (result, (player, soc)) => soc == socket ? Some(player) : result,
       None,
     );
};

let removePlayerBySocket = (socketId, state) => {
  switch(maybeGetSocketPlayer(socketId, state)){
    | None => state
    | Some(playerId) => 
      {...state,
        players: 
          Quad.update( 
            playerId, 
            x => {...x, pla_name: Player.stringOfId(playerId), sock_id_maybe: None}, 
            state.players)
      }
  };
}


/* A game is considered empty if no player slot has a socket attached. */
let isEmpty = (state) => {
  Quad.(
    map(x => x.sock_id_maybe, state.players)
    |> toList
    |> Belt.List.every(_, Js.Option.isNone))
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
| FindPlayersPhase(_, _) => true
| _ => false;

let isFindSubsPhase = fun
| FindSubsPhase(_, _) => true
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
