[@bs.deriving abstract]
type options = {src: array(string)};

type howl;

[@bs.module "howler"] [@bs.new] external makeHowl: options => howl = "Howl";

[@bs.send] external play: howl => unit = "";
