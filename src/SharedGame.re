open AppPrelude;

[@bs.val] external env_settings: Js.Nullable.t(string) = "process.env.SETTINGS";

let settings = 
  Settings.fromString(env_settings 
  |> Js.Nullable.toOption 
  |> Js.Option.getWithDefault("default"));

[@decco] type notis = list(Noti.t);

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

/*[@decco] won't work. decco doesn'nt yet support recursive types
  Follow at:https://github.com/ryb73/ppx_decco/issues/6 
*/
type phase =
  | IdlePhase
  | FindSubsPhase(int, phase)
  | FindPlayersPhase(int)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase(Player.id)
  | PackDepletedPhase
  | GameOverPhase;


let rec phase_encode =
  fun
  | IdlePhase => Js.Json.string("idle-phase")
  | FindSubsPhase(n, phase) =>
    Js.Json.array([|
      Js.Json.string("find-subs-phase"),
      n |> float_of_int |> Js.Json.number,
      phase_encode(phase),
    |])
  | FindPlayersPhase(n) =>
    Js.Json.array([|Js.Json.string("find-players-phase"), Js.Json.number(n |> float_of_int)|])
  | DealPhase => Js.Json.string("deal-phase")
  | BegPhase => Js.Json.string("beg-phase")
  | GiveOnePhase => Js.Json.string("give-one-phase")
  | RunPackPhase => Js.Json.string("run-pack-phase")
  | PlayerTurnPhase(playerId) => Js.Json.string("player-turn-phase-" ++ Player.stringifyId(playerId))
  | PackDepletedPhase => Js.Json.string("pack-depleted-phase")
  | GameOverPhase => Js.Json.string("game-over-phase");

let rec phase_decode = json => {
  switch(Js.Json.classify(json)){
    | Js.Json.JSONString(str_phase) => 
      switch(str_phase){
        | "idle-phase" => Belt.Result.Ok(IdlePhase)
        | "deal-phase" => Belt.Result.Ok(DealPhase)
        | "beg-phase" => Belt.Result.Ok(BegPhase)
        | "give-one-phase" => Belt.Result.Ok(GiveOnePhase)
        | "run-pack-phase" => Belt.Result.Ok(RunPackPhase)
        | "player-turn-phase-P1" => Belt.Result.Ok(PlayerTurnPhase(P1))
        | "player-turn-phase-P2" => Belt.Result.Ok(PlayerTurnPhase(P2))
        | "player-turn-phase-P3" => Belt.Result.Ok(PlayerTurnPhase(P3))
        | "player-turn-phase-P4" => Belt.Result.Ok(PlayerTurnPhase(P4))
        | "pack-depleted-phase" => Belt.Result.Ok(PackDepletedPhase)
        | "game-over-phase" => Belt.Result.Ok(GameOverPhase)
        | _ => Decco.error("Failed to decode phase classified as string: " ++ str_phase, json)
      }
    | Js.Json.JSONArray(jsonTs) => 
      switch(jsonTs){
        | [|_findPlayersPhase, n|] => 
          FindPlayersPhase(Js.Json.decodeNumber(n) |> Js.Option.getExn |> int_of_float) -> Belt.Result.Ok
        | [|_findSubsPhase, n, jsonPhase|] => 
          switch(phase_decode(jsonPhase)){
            | Belt.Result.Error(_) => Decco.error("Failed to recursively decode FindSubsPhase.", json)
            | Belt.Result.Ok(phase) => FindSubsPhase(Js.Json.decodeNumber(n) |> Js.Option.getExn |> int_of_float, phase)->Belt.Result.Ok
          }
          
        | _ => Decco.error("Failed to decode phase classified as array.", json)
      }
    | _ => Decco.error("Failed to decode phase. Json was not classified as expected.", json)
  }
};

let rec stringOfPhase = fun
  | IdlePhase => "IdlePhase"
  | FindSubsPhase(n, phase) => "FindSubsPhase(" ++ string_of_int(n) ++ ", " ++ stringOfPhase(phase) ++ ")"
  | FindPlayersPhase(n) => "FindPlayersPhase(" ++ string_of_int(n) ++ ")"
  | DealPhase => "DealPhase"
  | BegPhase => "BegPhase"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | PlayerTurnPhase(playerId) => "PlayerTurnPhase(" ++ Player.stringifyId(playerId) ++ ")"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase => "GameOverPhase";


let debugPhase = (phase, ~depth=0, ()) => {
  debuggin("phase: ", ~depth, ());
  let depth = depth + 1;
  debuggin(stringOfPhase(phase), ~depth, ())
};

let isFaceDownPhase = fun
| FindSubsPhase(_, BegPhase) | BegPhase 
| FindSubsPhase(_, GiveOnePhase) | GiveOnePhase => true
| _ => false
