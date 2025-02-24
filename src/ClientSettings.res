@spice
type volume = Mute(float) | Level(float)

@spice
type profileType =
  | Masculine
  | Feminine
  | Machine
  | Abstract

@spice
type sortHand = bool

@spice
type t = {
  volume: volume,
  client_id: string,
  client_profile_type: profileType,
  client_initials: string,
  sort_hand: sortHand,
}

let client_idDefault = Nanoid.nanoid()

let defaults = {
  volume: Level(0.5),
  client_id: client_idDefault,
  client_profile_type: Masculine,
  client_initials: "",
  sort_hand: false,
}
