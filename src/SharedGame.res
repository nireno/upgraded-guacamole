@val external env_settings: Js.Nullable.t<string> = "process.env.SETTINGS"

let settings = Settings.fromString(
  env_settings |> Js.Nullable.toOption |> Js.Option.getWithDefault("default"),
)

@decco
type privateGameContext = {
  private_game_key: string,
  private_game_host: Quad.id,
}

@decco
type game_id =
  | Public(string)
  | Private(privateGameContext)

@decco
type rematchDecision =
  | RematchUnknown
  | RematchAccepted
  | RematchDenied

@decco
type rematchDecisions = Quad.t<rematchDecision>

let isRematchDecisionKnown = x =>
  switch x {
  | RematchUnknown => false
  | RematchAccepted
  | RematchDenied => true
  }

let rematchDecisionIsUnknown = x =>
  switch x {
  | RematchUnknown => true
  | RematchAccepted
  | RematchDenied => false
  }

let rematchDecisionIsDenied = x =>
  switch x {
  | RematchDenied => true
  | RematchAccepted
  | RematchUnknown => false
  }

/* The rematch is is ready to start when all players have made a decision */
let isRematchPrimed = Quad.every(isRematchDecisionKnown, _)
let isRematchAcceptedByAll = Quad.every(decision =>
  switch decision {
  | RematchAccepted => true
  | _ => false
  }
, _)

let countRematchUnknown = rematchDecisions =>
  rematchDecisions->Quad.countHaving(rematchDecisionIsUnknown)

let countRematchDenied = rematchDecisions =>
  rematchDecisions->Quad.countHaving(rematchDecisionIsDenied)

let stringOfGameId = x =>
  switch x {
  | Public(key)
  | Private({private_game_key: key}) => key
  }

let debugOfGameId = x =>
  switch x {
  | Public(key) => j`Public($key)`
  | Private({private_game_key: key, private_game_host: quadId}) =>
    let quadIdText = quadId->Quad.stringifyId
    j`Private($key, $quadIdText)`
  }

let kickPoints = {
  open Card.Rank

  x =>
    switch x {
    | Ace => 1
    | Six => 2
    | Jack => 3
    | _ => 0
    }
}

@decco
type teamState = {
  team_score: int,
  team_points: int /* "Game" points */,
}

let initialTeamState = {
  team_score: 0,
  team_points: 0,
}

let teamOfPlayer = x =>
  switch x {
  | Quad.N1
  | N3 =>
    Team.T1
  | N2
  | N4 =>
    Team.T2
  }

@decco
type idleReason = DelayTrickCollection
// | DelayGameStart;
