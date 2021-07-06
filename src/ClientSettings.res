@decco
type volume = Mute(float) | Level(float)

@decco
type profileType =
  | Masculine
  | Feminine
  | Machine
  | Abstract

@decco
type sortHand = bool

let dicebearTypeOfProfileType = x =>
  switch x {
  | Masculine => "male"
  | Feminine => "female"
  | Machine => "bottts"
  | Abstract => "jdenticon"
  }

@decco
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
