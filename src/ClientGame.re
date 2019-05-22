open AppPrelude;
include SharedGame;

[@decco] type handFacing = | FaceUpHand(Hand.FaceUpHand.t) | FaceDownHand(Hand.FaceDownHand.t);
[@decco] type maybePlayerId = option(Player.id);
[@decco] type maybeTeamId = option(Team.id);
[@decco] type maybeTeamJackAward = option( (Team.id, award) );
[@decco] type maybeCard = option(Card.t);

type state = {
  gameId: string,
  phase: Player.phase,
  gamePhase: SharedGame.phase,
  p1Name: string,
  p2Name: string,
  p3Name: string,
  p4Name: string,
  me: Player.id,
  myTricks: list(Trick.t),
  dealer: Player.id,
  leader: Player.id,
  activePlayer: Player.id,
  activePlayerPhase: Player.phase,
  maybePlayerTurn: maybePlayerId,
  handFacing: handFacing,
  maybeLeadCard: maybeCard,
  maybeTrumpCard: maybeCard,
  board: list(Card.t),
  team1Points: int,
  team2Points: int,
  // team1GamePoints: int,
  // team2GamePoints: int,
  maybeTeamHigh: maybeTeamId,
  maybeTeamLow: maybeTeamId,
  maybeTeamJack: maybeTeamJackAward,
  maybeTeamGame: maybeTeamId,
};

let state_encode = state => {
  let keyValuePairs = [
    ("gameId", Js.Json.string(state.gameId)),
    ("phase", Player.phase_encode(state.phase)),
    ("gamePhase", SharedGame.phase_encode(state.gamePhase)),
    ("p1Name", Js.Json.string(state.p1Name)),
    ("p2Name", Js.Json.string(state.p2Name)),
    ("p3Name", Js.Json.string(state.p3Name)),
    ("p4Name", Js.Json.string(state.p4Name)),
    ("me", Player.id_encode(state.me)),
    ("myTricks", state.myTricks |> Json.Encode.list(Trick.t_encode)),
    ("dealer", Player.id_encode(state.dealer)),
    ("leader", Player.id_encode(state.leader)),
    ("activePlayer", Player.id_encode(state.activePlayer)),
    ("activePlayerPhase", Player.phase_encode(state.activePlayerPhase)),
    ("maybePlayerTurn", maybePlayerId_encode(state.maybePlayerTurn)),
    ("handFacing", handFacing_encode(state.handFacing)),
    ("maybeLeadCard", maybeCard_encode(state.maybeLeadCard)),
    ("maybeTrumpCard", maybeCard_encode(state.maybeTrumpCard)),
    ("board", state.board |> Json.Encode.list(Card.t_encode)),
    ("team1Points", Js.Json.number(state.team1Points |> float_of_int)),
    ("team2Points", Js.Json.number(state.team2Points |> float_of_int)),
    ("maybeTeamHigh", maybeTeamId_encode(state.maybeTeamHigh)),
    ("maybeTeamLow", maybeTeamId_encode(state.maybeTeamLow)),
    ("maybeTeamJack", maybeTeamJackAward_encode(state.maybeTeamJack)),
    ("maybeTeamGame", maybeTeamId_encode(state.maybeTeamGame)),
  ];
  
  keyValuePairs |> Js.Dict.fromList |> Js.Json.object_
};

let state_decode = json => {
  let jsonDecoder = (deccoDecoder, json) => {
    switch (json |> deccoDecoder) {
    | Belt.Result.Error(_) =>
      let strJson = Js.Json.stringifyAny(json) |> Js.Option.getWithDefault("json");
      raise @@ Json.Decode.DecodeError("Failed to decode " ++ strJson);
    | Belt.Result.Ok(result) => result
    };
  };

  let playerIdDecoder = jsonDecoder(Player.id_decode);
  let playerPhaseDecoder = jsonDecoder(Player.phase_decode);
  let maybeCardDecoder = jsonDecoder(maybeCard_decode);
  let maybeTeamIdDecoder = jsonDecoder(maybeTeamId_decode);

  try (
    Belt.Result.Ok(
      Json.Decode.{
        gameId: field("gameId", string, json),
        phase: field("phase", playerPhaseDecoder, json),
        gamePhase: field("gamePhase", jsonDecoder(SharedGame.phase_decode), json),
        p1Name: field("p1Name", string, json),
        p2Name: field("p2Name", string, json),
        p3Name: field("p3Name", string, json),
        p4Name: field("p4Name", string, json),
        me: field("me", playerIdDecoder, json),
        myTricks: field("myTricks", Json.Decode.list(jsonDecoder(Trick.t_decode)), json),
        dealer: field("dealer", playerIdDecoder, json),
        leader: field("leader", playerIdDecoder, json),
        activePlayer: field("activePlayer", playerIdDecoder, json),
        activePlayerPhase: field("activePlayerPhase", playerPhaseDecoder, json),
        maybePlayerTurn: field("maybePlayerTurn", jsonDecoder(maybePlayerId_decode), json),
        handFacing: field("handFacing", jsonDecoder(handFacing_decode), json),
        maybeLeadCard: field("maybeLeadCard", maybeCardDecoder, json),
        maybeTrumpCard: field("maybeTrumpCard", maybeCardDecoder, json),
        board: field("board", Json.Decode.list(jsonDecoder(Card.t_decode)), json),
        team1Points: field("team1Points", Json.Decode.int, json),
        team2Points: field("team2Points", Json.Decode.int, json),
        maybeTeamHigh: field("maybeTeamHigh", maybeTeamIdDecoder, json),
        maybeTeamLow: field("maybeTeamLow", maybeTeamIdDecoder, json),
        maybeTeamJack: field("maybeTeamJack", jsonDecoder(maybeTeamJackAward_decode), json),
        maybeTeamGame: field("maybeTeamGame", maybeTeamIdDecoder, json),
      },
    )
  ) {
  | Json.Decode.DecodeError(str) => Belt.Result.Error(str)
  };
};


let initialState = {
  gameId: "",
  phase: PlayerIdlePhase,
  gamePhase: FindPlayersPhase(3),
  p1Name: Player.stringOfId(P1),
  p2Name: Player.stringOfId(P2),
  p3Name: Player.stringOfId(P3),
  p4Name: Player.stringOfId(P4),
  me: P1,
  myTricks: [],
  dealer: P1,
  leader: P1,
  activePlayer: P1,
  activePlayerPhase: PlayerIdlePhase,
  maybePlayerTurn: None,
  handFacing: FaceDownHand(0),
  maybeLeadCard: None,
  maybeTrumpCard: None,
  board: [],
  team1Points: 0,
  team2Points: 0,
  maybeTeamHigh: None,
  maybeTeamLow: None,
  maybeTeamJack: None,
  maybeTeamGame: None,
};

type action =
  | MatchServerState(state)

let reducer = (_state, action) => {
  switch (action) {
  | MatchServerState(state) => state
  };
};

let getPlayerName = (playerId, state) => {
  switch(playerId){
    | Player.P1 => state.p1Name
    | Player.P2 => state.p2Name
    | Player.P3 => state.p3Name
    | Player.P4 => state.p4Name
  }
}

let stringOfState = (state) => {
  "ClientGame.state."
    ++ "{" ++ str_crlf
    ++ str_tab ++ "phase: " ++ Player.stringOfPhase(state.phase) ++ str_crlf
    ++ str_tab ++ "gamePhase: " ++ SharedGame.stringOfPhase(state.gamePhase) ++ str_crlf
    ++ "}" ++ str_crlf
}

let debugState = (state, ~ctx="", ~n=0, ()) => {
  if(ctx != "") {
    Js.log(ctx->leftPad(~n, ()))
  }
  Js.log(state->stringOfState->leftPad(~n=n+1, ()))
}
