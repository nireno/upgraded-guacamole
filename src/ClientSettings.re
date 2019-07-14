[@decco]
type volume = Mute(float) | Level(float);

[@decco]
type t = {
  allowSubbing: bool,
  volume,
};

let defaults = {
  allowSubbing: false,
  volume: Level(0.5),
};
