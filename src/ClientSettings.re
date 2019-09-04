[@decco]
type volume = Mute(float) | Level(float);

[@decco]
type t = {
  volume,
  client_id: string,
};

let client_idDefault = Nanoid.nanoid();

let defaults = {
  volume: Level(0.5),
  client_id: client_idDefault,
};
