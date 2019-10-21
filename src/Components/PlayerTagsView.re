[@react.component]
let make = (~isDealer, ~className=?, ~style=?) => {
  let dealerImgOpacity = isDealer ? "1" : "0";
  <div ?className ?style>
    <img
      className="player-tag__dealer__img player-tags__item"
      src="./static/img/emoji_pack.svg"
      style={ReactDOMRe.Style.make(~opacity=dealerImgOpacity, ())}
    />
  </div>;
};
