[@bs.deriving abstract]
type options = {
  src: array(string),
  [@bs.optional] volume: float,
};

type howl;

[@bs.module "howler"] [@bs.new] external makeHowl: options => howl = "Howl";

[@bs.send] external play: howl => unit = "";
[@bs.send] external rate: howl => float => unit = "";
[@bs.send] external playing: howl => bool = "";
[@bs.send] external volume: howl => float => unit = "";
