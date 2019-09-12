[@bs.val] external env_settings: Js.Nullable.t(string) = "process.env.SETTINGS";

let settings = 
  Settings.fromString(env_settings 
  |> Js.Nullable.toOption 
  |> Js.Option.getWithDefault("default"));

[@decco]
type game_id =
  | Public(string)
  | Private(string);

[@decco]
type rematchDecision = 
  | RematchUnknown
  | RematchAccepted
  | RematchDenied;

[@decco]
type rematchDecisions = Quad.t(rematchDecision);

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
