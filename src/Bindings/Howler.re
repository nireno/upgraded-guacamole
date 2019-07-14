[@bs.deriving abstract]
type options = {
  src: array(string),
  volume: float,
};

type howl;

[@bs.module "howler"] [@bs.new] external makeHowl: options => howl = "Howl";

[@bs.send] external play: howl => unit = "";
[@bs.send] external rate: howl => float => unit = "";
