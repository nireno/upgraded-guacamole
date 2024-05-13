type t =
  | ThumbsUp
  | ThumbsDown
  | Spades
  | Hearts
  | Clubs
  | Diamonds
  | Beaming
  | Crying
  | Jack
  | Ten

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
  | Jack => "emoji_winking_face.svg"
  | Ten => "emoji_fist.svg"
  }
