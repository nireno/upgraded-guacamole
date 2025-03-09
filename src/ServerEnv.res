@val external nodeEnv: string = "process.env.NODE_ENV"
@val external allFoursDebugPreset: Js.Nullable.t<string> = "process.env.allfours_debug_preset"
@val external httpPortEnv: Js.Nullable.t<string> = "process.env.allfours_port"
@val external adminPasswordEnv: Js.Nullable.t<string> = "process.env.allfours_admin_password"
@val external kickPlayerSeconds: Js.Nullable.t<int> = "process.env.allfours_kick_player_seconds"
@val
external gameStartingCountdownSeconds: Js.Nullable.t<int> =
  "process.env.allfours_game_starting_countdown_seconds"

let kickPlayerSeconds = Js.Option.getWithDefault(60, Js.Nullable.toOption(kickPlayerSeconds))
let gameStartingCountdownSeconds = Js.Option.getWithDefault(
  5,
  Js.Nullable.toOption(gameStartingCountdownSeconds),
)
