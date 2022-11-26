@ocaml.doc(" For notification messages such as \"player left the game\" ") @decco
type level = Success | Info | Warning | Danger

@decco
type kind = Confirm | Duration(int)

@decco
type roundSummary = {
  noti_maybeTeamHigh: option<GameAward.luckyAwardData>,
  noti_maybeTeamLow: option<GameAward.luckyAwardData>,
  noti_maybeTeamJack: option<GameAward.jackAwardData>,
  noti_maybeTeamGame: option<GameAward.gameAwardData>,
}

@decco
type message = Text(string) | RoundSummary(roundSummary)

@decco
type t = {
  noti_id: string,
  noti_recipient: Player.id,
  noti_level: level,
  noti_message: message,
  noti_kind: kind,
}

@ocaml.doc(" 
  Helper for broadcasting information from one player to all other players.
")
let playerBroadcast = (~from: Player.id, ~msg, ~level=Info, ~kind=Duration(3375), ()) =>
  List.filter(playerId => playerId != from, list{Quad.N1, N2, N3, N4}) |> List.map(playerId => {
    noti_id: Nanoid.nanoid(),
    noti_recipient: playerId,
    noti_message: msg,
    noti_level: level,
    noti_kind: kind,
  })

let broadcast = (~msg, ~level=Info, ~kind=Duration(3375), ()) =>
  list{Quad.N1, N2, N3, N4} |> List.map(playerId => {
    noti_id: Nanoid.nanoid(),
    noti_recipient: playerId,
    noti_message: msg,
    noti_level: level,
    noti_kind: kind,
  })

module State = {
  let initial = list{}

  type action =
    | Add(list<t>)
    | AddOne(t)
    | Remove(t)
    | RemoveKind(kind)
    | Reset(list<t>)

  let reducer = (prevNotis, action) =>
    switch action {
    | Add(notis) => Belt.List.concat(prevNotis, notis)
    | AddOne(noti) => Belt.List.concat(prevNotis, list{noti})
    | Remove(notiToRemove) => List.filter(noti => noti != notiToRemove, prevNotis)
    | RemoveKind(kind) => Belt.List.keep(prevNotis, noti => noti.noti_kind != kind)
    | Reset(notis) => notis
    }
}
