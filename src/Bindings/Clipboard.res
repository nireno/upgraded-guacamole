type t
type trigger
type event
type stringOfTrigger = trigger => string

@deriving(abstract)
type options = {text: stringOfTrigger}

@module @new external make: (string, options) => t = "clipboard"
@send external on: (t, string, event => unit) => unit = "on"
@send external off: (t, string) => unit = "off"
