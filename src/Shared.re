module ActivePlayer = Shared__ActivePlayer;
module Game = Shared__Game;

let normalizeInviteCode = code =>
  // Lowercases all letters and removes spaces
  Js.String.toLowerCase(code) |> Js.String.replaceByRe([%re "/ /g"], "");