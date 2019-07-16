include SharedGame;

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
  game_id: game_id,
  deck: Deck.t,
  players: (playerState, playerState, playerState, playerState),
  teams: (teamState, teamState),
  notis: notis,
  maybeTrumpCard: option(Card.t),
  maybeLeadCard: option(Card.t),
  dealer: Player.id,
  leader: Player.id,
  maybeTeamHigh: option(Team.id),
  maybeTeamLow: option(Team.id),
  maybeTeamJack: option((Team.id, GameAward.award)),
  maybeTeamGame: option(Team.id),
  phase,
  maybeKickTimeoutId: option(Js.Global.timeoutId),
};

module SockServ = BsSocket.Server.Make(SocketMessages);

let debugOfState = (state) => {
  let stringOfPlayer = player => {
    let name = player.pla_name;
    let socket = Belt.Option.mapWithDefault(player.pla_socket, "None", SockServ.Socket.getId);
    let card = Card.codeOfMaybeCard(player.pla_card);
    let tricks =
      List.map(Trick.codeOfTrick, player.pla_tricks)
      |> Belt.List.toArray
      |> Js.Array.joinWith(", ");

    {j|{$name, $socket, $card, [$tricks] }|j};
  };

  let debugOfPlayers = {
    "Player1": Quad.select(N1, stringOfPlayer, state.players),
    "Player2": Quad.select(N2, stringOfPlayer, state.players),
    "Player3": Quad.select(N3, stringOfPlayer, state.players),
    "Player4": Quad.select(N4, stringOfPlayer, state.players),
  };

  {
    "game_id": state.game_id,
    "phase": stringOfPhase(state.phase),
    "dealer": Player.stringOfId(state.dealer),
    "leader": Player.stringOfId(state.leader),
    "maybeTrumpCard": Card.codeOfMaybeCard(state.maybeTrumpCard),
    "maybeLeadCard": Card.codeOfMaybeCard(state.maybeLeadCard),
    "players": debugOfPlayers,
  }
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
    maybeTeamGame: None,
    phase: FindPlayersPhase(4),
    maybeKickTimeoutId: None,
  };
};

let getAllPlayerSockets = state => {
  let rec f:
    (Player.id, list((Player.id, BsSocket.Server.socketT))) =>
    list((Player.id, BsSocket.Server.socketT)) =
    (player, playerSockets) => {
      let playerSockets =
        switch (Quad.get(player, state.players).pla_socket) {
        | None => playerSockets
        | Some(socket) => [(player, socket), ...playerSockets]
        };
      player == N4 ? playerSockets : f(Quad.nextId(player), playerSockets);
    };
  f(N1, []);
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
  switch(Quad.toDict(state.players) |> List.filter(((_k:Player.id, v:playerState)) => Js.Option.isNone(v.pla_socket))){
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
