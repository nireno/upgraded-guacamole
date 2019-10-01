[@bs.deriving abstract]
type options = {
  src: array(string),
  [@bs.optional] volume: float,
};

type howl;

[@bs.module "howler"] [@bs.new] external makeHowl: options => howl = "Howl";

[@bs.send] external play: howl => unit = "play";
[@bs.send] external rate: howl => float => unit = "rate";
[@bs.send] external playing: howl => bool = "playing";
[@bs.send] external volume: howl => float => unit = "volume";
