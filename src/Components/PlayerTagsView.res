@react.component
let make = (~isDealer, ~className=?, ~style=?) => {
  let dealerImgOpacity = isDealer ? "1" : "0"
  <div ?className ?style>
    <div
      className="player-tag__dealer__img player-tags__item"
      style={ReactDOM.Style.make(~opacity=dealerImgOpacity, ())}>
      <Svg_Emoji_Pack />
    </div>
  </div>
}
