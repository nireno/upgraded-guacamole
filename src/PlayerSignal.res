type t =
  | ThumbsUp
  | ThumbsDown
  | Spades
  | Hearts
  | Clubs
  | Diamonds
  | Beaming
  | Crying

let toFileName = x =>
  switch x {
  | ThumbsUp => "emoji_thumbs_up.svg"
  | ThumbsDown => "emoji_thumbs_down.svg"
  | Spades => "spade.svg"
  | Hearts => "heart.svg"
  | Clubs => "club.svg"
  | Diamonds => "diamond.svg"
  | Beaming => "emoji_beaming.svg"
  | Crying => "emoji_crying.svg"
  }
