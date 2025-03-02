@react.component
let make = (~signal: PlayerSignal.t) => {
  switch signal {
  | ThumbsUp => <Svg_Emoji_ThumbsUp />
  | ThumbsDown => <Svg_Emoji_ThumbsDown />
  | Spades => <Svg_Emoji_Spades />
  | Hearts => <Svg_Emoji_Hearts />
  | Clubs => <Svg_Emoji_Clubs />
  | Diamonds => <Svg_Emoji_Diamonds />
  | Beaming => <Svg_Emoji_Beaming />
  | Crying => <Svg_Emoji_Crying />
  | Jack => <Svg_Emoji_WinkingFace />
  | Ten => <Svg_Emoji_Fist />
  }
}
