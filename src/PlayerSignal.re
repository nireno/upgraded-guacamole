type t = 
  | ThumbsUp
  | ThumbsDown
  ;

let toFileName =
  fun
  | ThumbsUp => "emoji_thumbs_up.svg"
  | ThumbsDown => "emoji_thumbs_down.svg"
  ;