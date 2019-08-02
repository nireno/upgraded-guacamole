type t;
type trigger;
type event;
type stringOfTrigger = trigger => string;

[@bs.deriving abstract]
type options = {
  text: stringOfTrigger
};

[@bs.module][@bs.new] external make: (string, options) => t = "clipboard";
[@bs.send] external on: (t, string, event => unit) => unit = ""
