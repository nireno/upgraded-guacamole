@ocaml.doc(" For notification messages such as \"player left the game\" ") @spice
type level = Success | Info | Warning | Danger

@spice
type kind = Confirm | Duration(int)

@spice
type roundSummary = {
  noti_maybeTeamHigh: option<GameAward.luckyAwardData>,
  noti_maybeTeamLow: option<GameAward.luckyAwardData>,
  noti_maybeTeamJack: option<GameAward.jackAwardData>,
  noti_maybeTeamGame: option<GameAward.gameAwardData>,
}

@spice
type message = Text(string) | RoundSummary(roundSummary)

@spice
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
  List.map(playerId => {
    noti_id: Nanoid.nanoid(),
    noti_recipient: playerId,
    noti_message: msg,
    noti_level: level,
    noti_kind: kind,
  }, List.filter(playerId => playerId != from, list{Quad.N1, N2, N3, N4}))

let broadcast = (~msg, ~level=Info, ~kind=Duration(3375), ()) => List.map(playerId => {
    noti_id: Nanoid.nanoid(),
    noti_recipient: playerId,
    noti_message: msg,
    noti_level: level,
    noti_kind: kind,
  }, list{Quad.N1, N2, N3, N4})

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
