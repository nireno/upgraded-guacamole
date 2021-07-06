module ActivePlayer = Shared__ActivePlayer
module Game = Shared__Game

@val external baseUrlEnv: Js.Nullable.t<string> = "process.env.allfours_base_url"

let normalizeInviteCode = code =>
  // Lowercases all letters and removes spaces
  Js.String.toLowerCase(code) |> Js.String.replaceByRe(%re("/ /g"), "")
