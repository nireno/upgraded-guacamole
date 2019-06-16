open AppPrelude; 
include SharedGame;


type action =
  | Noop
  | PlayCard(Player.id, Card.t)
  | BlockPlay(Player.id)
  | EndTrick
  | AdvanceRound
  | NewRound
  | Beg
  | Stand
  | GiveOne
  | Deal
  | RunPack
  | DealAgain
  | LeaveGame(Player.id)
  | ClearNotis
  | CheatPoints(Team.id, int);

type playerState = {
  pla_socket: option(BsSocket.Server.socketT),
  pla_name: string,
  pla_hand: Hand.FaceUpHand.t,
  pla_tricks: list(Trick.t),
  pla_card: option(Card.t), /* Card on board */
}

let initialPlayerState = playerId => {
  pla_socket: None,
  pla_name: Player.stringOfId(playerId),
  pla_hand: [],
  pla_tricks: [],
  pla_card: None,
};

type state = {
  roomKey: string,
  deck: Deck.t,
  players: (playerState, playerState, playerState, playerState),
  teams: (teamState, teamState),
  notis: notis,
  maybeTrumpCard: option(Card.t),
  maybeLeadCard: option(Card.t),
  dealer: Player.id,
  leader: Player.id,
  maybePlayerTurn: option(Player.id),
  maybeTeamHigh: option(Team.id),
  maybeTeamLow: option(Team.id),
  maybeTeamJack: option((Team.id, GameAward.award)),
  maybeTeamGame: option(Team.id),
  phase,
};

module SockServ = BsSocket.Server.Make(SocketMessages);

let stringOfMaybeSocket = fun
  | None => "None"
  | Some(socket) => SockServ.Socket.getId(socket);

let stringOfState = (state) => {
  "Game.state."
    ++ "{" ++ str_crlf
    ++ str_tab ++ "socketIds: [" ++ str_crlf
    ++ str_tab ++ str_tab  ++ ( GamePlayers.get(Player.P1, state.players).pla_socket |> stringOfMaybeSocket ) ++ ", " ++ str_crlf
    ++ str_tab ++ str_tab  ++ ( GamePlayers.get(Player.P2, state.players).pla_socket |> stringOfMaybeSocket ) ++ ", " ++ str_crlf
    ++ str_tab ++ str_tab  ++ ( GamePlayers.get(Player.P3, state.players).pla_socket |> stringOfMaybeSocket ) ++ ", " ++ str_crlf
    ++ str_tab ++ str_tab  ++ ( GamePlayers.get(Player.P4, state.players).pla_socket |> stringOfMaybeSocket ) ++ str_crlf
    ++ str_tab ++ "]" ++ str_crlf
    ++ str_tab ++ "roomKey: " ++ state.roomKey ++ str_crlf
    ++ str_tab ++ "phase: " ++ stringOfPhase(state.phase) ++ str_crlf
    ++ str_tab ++ "dealer: " ++ Player.stringOfId(state.dealer) ++ str_crlf
    ++ str_tab ++ "leader: " ++ Player.stringOfId(state.leader) ++ str_crlf
    ++ str_tab ++ "maybePlayerTurn: " ++ Player.stringOfMaybeId(state.maybePlayerTurn) ++ str_crlf
    ++ "}" ++ str_crlf
};

let initialState = () => {
  {
    roomKey: "",
    deck: Deck.make() |> Deck.shuffle,
    players: (
      initialPlayerState(P1),
      initialPlayerState(P2),
      initialPlayerState(P3),
      initialPlayerState(P4),
    ),
    teams: (initialTeamState, initialTeamState),
    notis: [],
    maybePlayerTurn: None,
    maybeTrumpCard: None,
    maybeLeadCard: None,
    dealer: P1,
    leader: P2,
    maybeTeamHigh: None,
    maybeTeamLow: None,
    maybeTeamJack: None,
    maybeTeamGame: None,
    phase: FindPlayersPhase(4),
  };
};

let getAllPlayerSockets = state => {
  let rec f:
    (Player.id, list((Player.id, BsSocket.Server.socketT))) =>
    list((Player.id, BsSocket.Server.socketT)) =
    (player, playerSockets) => {
      let playerSockets =
        switch (GamePlayers.get(player, state.players).pla_socket) {
        | None => playerSockets
        | Some(socket) => [(player, socket), ...playerSockets]
        };
      player == P4 ? playerSockets : f(Player.nextPlayer(player), playerSockets);
    };
  f(P1, []);
};

let playerCount = state => {
  let (n1, n2, n3, n4) =
    Quad.map(x => Js.Option.isSome(x.pla_socket) ? 1 : 0, state.players);
  n1 + n2 + n3 + n4;
};

let countPlayers = players => {
  let (n1, n2, n3, n4) = 
    Quad.map(x => Js.Option.isSome(x.pla_socket) ? 1 : 0, players);
  n1 + n2 + n3 + n4;
};

let findEmptySeat = state => {
  switch(GamePlayers.toDict(state.players) |> List.filter(((_k:Player.id, v:playerState)) => Js.Option.isNone(v.pla_socket))){
    | [] => None
    | [(pla_id, _v), ..._rest] => Some(pla_id)
  }
};


let isGameOverTest = state => {
  GameTeams.get(T1, state.teams).team_score >= SharedGame.settings.winningScore
  || GameTeams.get(T2, state.teams).team_score >= SharedGame.settings.winningScore;
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

let removePlayerBySocket = (socketId, state) => {
  switch(maybeGetSocketPlayer(socketId, state)){
    | None => state
    | Some(playerId) => 
      {...state,
        players: 
          GamePlayers.update( 
            playerId, 
            x => {...x, pla_name: Player.stringOfId(playerId), pla_socket: None}, 
            state.players)
      }
  };
}


/* A game is considered empty if no player slot has a socket attached. */
let isEmpty = (state) => {
  Quad.(
    map(x => x.pla_socket, state.players)
    |> toList
    |> Belt.List.every(_, Js.Option.isNone))
};

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
                  : dealer == player && gamePhase == PackDepletedPhase
                      ? (player, PlayerRedealPhase)
                      : leader == player && gamePhase == BegPhase
                          ? (player, PlayerBegPhase) : (player, PlayerIdlePhase);
  };


let debugState = (state, ~ctx="", ~n=0, ()) => {
  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(state->stringOfState->leftPad(~n=n+1, ()))
}
