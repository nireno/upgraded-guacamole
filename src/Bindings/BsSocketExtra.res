type roomKey = string
type socketId = roomKey

module AdapterRoom = {
  type t

  @get external sockets: t => Js.Dict.t<bool> = "sockets"
  @get external length: t => int = "length"

  let contains: (socketId, t) => bool = (socketId, room) =>
    (Belt.Array.some(_, key => key == socketId))(Js.Dict.keys(sockets(room)))

  let isEmpty: t => bool = room => length(room) == 0
}

module Adapter = {
  type t
  type roomsDict = Js.Dict.t<AdapterRoom.t>

  @get external rooms: t => roomsDict = "rooms"

  let getRoom: (roomKey, t) => option<AdapterRoom.t> = (roomKey, adapter) =>
    (Js.Dict.get(_, roomKey))(rooms(adapter))
}
