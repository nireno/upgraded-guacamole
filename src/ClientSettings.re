[@decco]
type volume = Mute(float) | Level(float);

[@decco]
type t = {
  volume,
};

let defaults = {
  volume: Level(0.5),
};
