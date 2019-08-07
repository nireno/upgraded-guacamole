[@bs.val] external env_settings: Js.Nullable.t(string) = "process.env.SETTINGS";

let settings = 
  Settings.fromString(env_settings 
  |> Js.Nullable.toOption 
  |> Js.Option.getWithDefault("default"));

[@decco]
type game_id =
  | Public(string)
  | Private(string);

let stringOfGameId =
  fun
  | Public(str)
  | Private(str) => str;


let kickPoints =
  Card.Rank.(
    fun
    | Ace => 1
    | Six => 2
    | Jack => 3
    | _ => 0
  );


[@decco]
type teamState = {
  team_score: int,
  team_points: int, /* "Game" points */
};

let initialTeamState = {
  team_score: 0,
  team_points: 0,
};

let teamOfPlayer =
  fun
  | Quad.N1
  | N3 => Team.T1
  | N2
  | N4 => Team.T2;


/*[@decco] won't work. decco doesn'nt yet support recursive types
  Follow at:https://github.com/ryb73/ppx_decco/issues/6 
*/
type phase =
  | IdlePhase
  | FindSubsPhase(int, phase)
  | FindPlayersPhase(
      int, /* numEmptySeats: Number of empty seats in this game */
      bool, /* canSub: There exists a public game in FindSubsPhase */
    )
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase;

let isPlayerActivePhase = fun
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(_) 
  | PackDepletedPhase => true
  | IdlePhase 
  | FindSubsPhase(_, _)
  | FindPlayersPhase(_, _)
  | GameOverPhase => false;

let rec phase_encode =
  fun
  | IdlePhase => Js.Json.string("idle-phase")
  | FindSubsPhase(n, phase) =>
    Js.Json.array([|
      Js.Json.string("find-subs-phase"),
      n |> float_of_int |> Js.Json.number,
      phase_encode(phase),
    |])
  | FindPlayersPhase(numEmptySeats, canSub) => {
      Js.Json.array([|
        Js.Json.string("find-players-phase"),
        Js.Json.number(numEmptySeats |> float_of_int),
        Js.Json.boolean(canSub),
      |]);
    }
  | DealPhase => Js.Json.string("deal-phase")
  | BegPhase => Js.Json.string("beg-phase")
  | GiveOnePhase => Js.Json.string("give-one-phase")
  | RunPackPhase => Js.Json.string("run-pack-phase")
  | PlayerTurnPhase(playerId) =>
    Js.Json.string("player-turn-phase-" ++ Quad.stringifyId(playerId))
  | PackDepletedPhase => Js.Json.string("pack-depleted-phase")
  | GameOverPhase => Js.Json.string("game-over-phase");


let rec phase_decode = json => {
  switch (Js.Json.classify(json)) {
  | Js.Json.JSONString(str_phase) =>
    switch (str_phase) {
    | "idle-phase" => Belt.Result.Ok(IdlePhase)
    | "deal-phase" => Belt.Result.Ok(DealPhase)
    | "beg-phase" => Belt.Result.Ok(BegPhase)
    | "give-one-phase" => Belt.Result.Ok(GiveOnePhase)
    | "run-pack-phase" => Belt.Result.Ok(RunPackPhase)
    | "player-turn-phase-N1" => Belt.Result.Ok(PlayerTurnPhase(N1))
    | "player-turn-phase-N2" => Belt.Result.Ok(PlayerTurnPhase(N2))
    | "player-turn-phase-N3" => Belt.Result.Ok(PlayerTurnPhase(N3))
    | "player-turn-phase-N4" => Belt.Result.Ok(PlayerTurnPhase(N4))
    | "pack-depleted-phase" => Belt.Result.Ok(PackDepletedPhase)
    | "game-over-phase" => Belt.Result.Ok(GameOverPhase)
    | _ => Decco.error("Failed to decode phase classified as string: " ++ str_phase, json)
    }
  | Js.Json.JSONArray(jsonTs) =>
    switch (jsonTs) {
    | [|phaseJson, numEmptySeats, canSub|]
        when
          Js.Json.decodeString(phaseJson) |> Js.Option.getWithDefault("") == "find-players-phase" =>
      FindPlayersPhase(
        Js.Json.decodeNumber(numEmptySeats) |> Js.Option.getExn |> int_of_float,
        Js.Json.decodeBoolean(canSub) |> Js.Option.getExn,
      )
      ->Belt.Result.Ok
    | [|phaseJson, n, subPhaseJson|]
        when Js.Json.decodeString(phaseJson) |> Js.Option.getWithDefault("") == "find-subs-phase" =>
      switch (phase_decode(subPhaseJson)) {
      | Belt.Result.Error(_) => Decco.error("Failed to recursively decode FindSubsPhase.", json)
      | Belt.Result.Ok(phase) =>
        FindSubsPhase(Js.Json.decodeNumber(n) |> Js.Option.getExn |> int_of_float, phase)
        ->Belt.Result.Ok
      }
    | _ => Decco.error("Failed to decode phase classified as array.", json)
    }
  | _ => Decco.error("Failed to decode phase. Json was not classified as expected.", json)
  };
};

let rec stringOfPhase = fun
  | IdlePhase => "IdlePhase"
  | FindSubsPhase(n, phase) => "FindSubsPhase(" ++ string_of_int(n) ++ ", " ++ stringOfPhase(phase) ++ ")"
  | FindPlayersPhase(numEmptySeats, canSub) => "FindPlayersPhase(" ++ string_of_int(numEmptySeats) ++ ", " ++ string_of_bool(canSub) ++ ")"
  | DealPhase => "DealPhase"
  | BegPhase => "BegPhase"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | PlayerTurnPhase(playerId) => "PlayerTurnPhase(" ++ Quad.stringifyId(playerId) ++ ")"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase => "GameOverPhase";


let isFaceDownPhase = fun
| FindSubsPhase(_, BegPhase) | BegPhase 
| FindSubsPhase(_, GiveOnePhase) | GiveOnePhase => true
| _ => false
