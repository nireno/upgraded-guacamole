open AppPrelude; 
include SharedGame;


type action =
  | Noop
  | PlayCard(Player.id, Card.t)
  | BlockPlay(Player.id)
  | EndTrick
  | AdvanceRound
  | NewRound
  | EndRound
  | Beg
  | Stand
  | GiveOne
  | Deal
  | RunPack
  | DealAgain
  | CheatPoints(Team.id, int);

type state = {
  room: BsSocketExtra.AdapterRoom.t,
  roomKey: string,
  deck: Deck.t,
  board: list(Card.t),
  p1Socket: option(BsSocket.Server.socketT),
  p2Socket: option(BsSocket.Server.socketT),
  p3Socket: option(BsSocket.Server.socketT),
  p4Socket: option(BsSocket.Server.socketT),
  p1Hand: Hand.FaceUpHand.t,
  p2Hand: Hand.FaceUpHand.t,
  p3Hand: Hand.FaceUpHand.t,
  p4Hand: Hand.FaceUpHand.t,
  p1Tricks: list(Trick.t),
  p2Tricks: list(Trick.t),
  p3Tricks: list(Trick.t),
  p4Tricks: list(Trick.t),
  maybeTrumpCard: option(Card.t), /* using slot suffix to denote an optional prop. */
  maybeLeadCard: option(Card.t),
  dealer: Player.id,
  leader: Player.id,
  maybePlayerTurn: option(Player.id),
  team1Points: int,
  team2Points: int,
  maybeTeamHigh: option(Team.id),
  maybeTeamLow: option(Team.id),
  maybeTeamJack: option((Team.id, award)),
  maybeTeamGame: option(Team.id),
  phase,
};

let stringOfState = (state) => {
  "Game.state."
    ++ "{" ++ str_crlf
    ++ str_tab ++ "roomKey: " ++ state.roomKey ++ str_crlf
    ++ str_tab ++ "phase: " ++ stringOfPhase(state.phase) ++ str_crlf
    ++ str_tab ++ "dealer: " ++ Player.stringOfId(state.dealer) ++ str_crlf
    ++ str_tab ++ "leader: " ++ Player.stringOfId(state.leader) ++ str_crlf
    ++ str_tab ++ "maybePlayerTurn: " ++ Player.stringOfMaybeId(state.maybePlayerTurn) ++ str_crlf
    ++ "}" ++ str_crlf
}

let initialState = () => {
  {
    roomKey: "",
    room: Js.null |> Obj.magic,
    deck: Deck.make() |> Deck.shuffle,
    board: [],
    p1Socket: None,
    p2Socket: None,
    p3Socket: None,
    p4Socket: None,
    p1Hand: [],
    p2Hand: [],
    p3Hand: [],
    p4Hand: [],
    p1Tricks: [],
    p2Tricks: [],
    p3Tricks: [],
    p4Tricks: [],
    maybePlayerTurn: None,
    maybeTrumpCard: None,
    maybeLeadCard: None,
    dealer: P1,
    leader: P2,
    team1Points: 0,
    team2Points: 0,
    maybeTeamHigh: None,
    maybeTeamLow: None,
    maybeTeamJack: None,
    maybeTeamGame: None,
    phase: FindPlayersPhase(4),
  };
};

let updateHand: (Player.id, Hand.FaceUpHand.t, state) => state =
  (player, hand, state) => {
    switch (player) {
    | P1 => {...state, p1Hand: hand}
    | P2 => {...state, p2Hand: hand}
    | P3 => {...state, p3Hand: hand}
    | P4 => {...state, p4Hand: hand}
    };
  };

let updatePlayerSocket: (Player.id, BsSocket.Server.socketT, state) => state =
  (player, socket, state) => {
    switch (player) {
    | P1 => {...state, p1Socket: Some(socket)}
    | P2 => {...state, p2Socket: Some(socket)}
    | P3 => {...state, p3Socket: Some(socket)}
    | P4 => {...state, p4Socket: Some(socket)}
    };
  };

let removePlayerSocket: (Player.id, state) => state =
  (player, state) => {
    switch (player) {
    | P1 => {...state, p1Socket: None}
    | P2 => {...state, p2Socket: None}
    | P3 => {...state, p3Socket: None}
    | P4 => {...state, p4Socket: None}
    };
  };

let getPlayerSocket = (player, state) => {
  switch (player) {
  | Player.P1 => state.p1Socket
  | P2 => state.p2Socket
  | P3 => state.p3Socket
  | P4 => state.p4Socket
  };
};

let getPlayerHand = (player, state) => {
  switch (player) {
  | Player.P1 => state.p1Hand
  | P2 => state.p2Hand
  | P3 => state.p3Hand
  | P4 => state.p4Hand
  };
}

let getAllPlayerSockets = state => {
  let rec f:
    (Player.id, list((Player.id, BsSocket.Server.socketT))) =>
    list((Player.id, BsSocket.Server.socketT)) =
    (player, playerSockets) => {
      let playerSockets =
        switch (getPlayerSocket(player, state)) {
        | None => playerSockets
        | Some(socket) => [(player, socket), ...playerSockets]
        };
      player == P4 ? playerSockets : f(Player.nextPlayer(player), playerSockets);
    };
  f(P1, []);
};

let playerCount = state => {
  let count = 0;
  let count = Js.Option.isSome(state.p1Socket) ? count + 1 : count;
  let count = Js.Option.isSome(state.p2Socket) ? count + 1 : count;
  let count = Js.Option.isSome(state.p3Socket) ? count + 1 : count;
  let count = Js.Option.isSome(state.p4Socket) ? count + 1 : count;
  count;
}

let findEmptySeat = state => {
  let rec f: Player.id => option(Player.id) =
    player => {
      switch (player) {
      | P1 =>
        Js.Option.isSome(state.p1Socket)
          ? f(Player.nextPlayer(player)) : Some(P1)
      | P2 =>
        Js.Option.isSome(state.p2Socket)
          ? f(Player.nextPlayer(player)) : Some(P2)
      | P3 =>
        Js.Option.isSome(state.p3Socket)
          ? f(Player.nextPlayer(player)) : Some(P3)
      | P4 => Js.Option.isSome(state.p4Socket) ? None : Some(P4)
      };
    };
  f(P1);
};


let isGameOverTest = state => {
  state.team1Points >= winningScore || state.team2Points >= winningScore;
};

let addPoints = (team, value, state) => {
  switch (team) {
  | Team.T1 => {...state, team1Points: state.team1Points + value}
  | T2 => {...state, team2Points: state.team2Points + value}
  };
};

/** The board is just a list of cards. It doesn't track which player played them.
But if I know which player is the leader, I can tell which card belongs to which player */
let playerBoardIndices = leader => {
  Player.(
    switch (leader) {
    | P1 => (0, 1, 2, 3)
    | P2 => (3, 0, 1, 2)
    | P3 => (2, 3, 0, 1)
    | P4 => (1, 2, 3, 0)
    }
  );
};

let trickToPlayerCards: Trick.t => list((Player.id, Card.t)) =
  trick => {
    [
      (P1, trick.p1Card),
      (P2, trick.p2Card),
      (P3, trick.p3Card),
      (P4, trick.p4Card),
    ];
  };

let trickContainsCard: (Card.t, Trick.t) => bool =
  (testCard, trick) => {
    trickToPlayerCards(trick)
    |> List.exists(((_, card)) => card == testCard);
  };


let playerOfIntUnsafe =
  fun
  | 1 => Player.P1
  | 2 => P2
  | 3 => P3
  | 4 => P4
  | n =>
    failwith("Expected a number in [1, 4] but got: " ++ string_of_int(n));


let hasSocket = ( socket, state ) => {
  getAllPlayerSockets(state)
  |> List.map(((_player, socket)) => socket)
  |> List.mem(socket)
}

let maybeGetSocketPlayer = (socket, state) => {
  getAllPlayerSockets(state)
  |> List.fold_left(
       (result, (player, soc)) => soc == socket ? Some(player) : result,
       None,
     );
};

let isEmpty = (state) => {
  [Player.P1, P2, P3, P4]
  |> List.map(player => getPlayerSocket(player, state))
  |> Belt.List.every(_, Js.Option.isNone)
}

let decidePlayerPhase:
  (phase, Player.id, Player.id, option(Player.id), Player.id) => (Player.id, Player.phase) =
  (gamePhase, dealer, leader, maybePlayerTurn, player) => {
    Player.maybeIdEqual(maybePlayerTurn, player)
      ? (player, Player.PlayerTurnPhase(player))
      : dealer == player && gamePhase == DealPhase
          ? (player, PlayerDealPhase)
          : dealer == player && gamePhase == GiveOnePhase
              ? (player, PlayerGiveOnePhase)
              : dealer == player && gamePhase == RunPackPhase
                  ? (player, PlayerRunPackPhase)
                  : leader == player && gamePhase == BegPhase
                      ? (player, PlayerBegPhase) : (player, PlayerIdlePhase);
  };


let debugState = (state, ~ctx="", ~n=0, ()) => {
  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(state->stringOfState->leftPad(~n=n+1, ()))
}
