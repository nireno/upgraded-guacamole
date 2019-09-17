type roomKey = string;
type socketId = roomKey;

module AdapterRoom = {
  type t;

  [@bs.get] external sockets: t => Js.Dict.t(bool) = "sockets";
  [@bs.get] external length: t => int = "length";

  let contains: (socketId, t) => bool =
    (socketId, room) => {
      room |> sockets |> Js.Dict.keys |> Belt.Array.some(_, key => key == socketId);
    };

  let isEmpty: t => bool = room => length(room) == 0;
};

module Adapter = {
  type t;
  type roomsDict = Js.Dict.t(AdapterRoom.t);

  [@bs.get] external rooms: t => roomsDict = "rooms";

  let getRoom: (roomKey, t) => option(AdapterRoom.t) =
    (roomKey, adapter) => {
      adapter |> rooms |> Js.Dict.get(_, roomKey);
    };
};
