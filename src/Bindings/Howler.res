@deriving(abstract)
type options = {
  src: array<string>,
  @optional volume: float,
}

type howl

@module("howler") @new external makeHowl: options => howl = "Howl"

@send external play: howl => unit = "play"
@send external rate: (howl, float) => unit = "rate"
@send external playing: howl => bool = "playing"
@send external volume: (howl, float) => unit = "volume"
