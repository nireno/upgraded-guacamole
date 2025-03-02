@react.component
let make = (~onToggleSortClick, ~onSignalClick, ~sortHand) => {
  let sortIcon = sortHand ? <Svg_Control_Sort /> : <Svg_Control_Unsort />
  <div className="toolbar-layers relative m-1">
    <div className="layer bg-white opacity-25" />
    <div className="toolbar relative grid grid-cols-12 gap-1 z-10 p-1">
      <div className="toolbar__button bg-white" onClick=onToggleSortClick> sortIcon </div>
      <div className="placeholder" />
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Spades)}>
        <Svg_Emoji_Spades />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Hearts)}>
        <Svg_Emoji_Hearts />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Clubs)}>
        <Svg_Emoji_Clubs />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Diamonds)}>
        <Svg_Emoji_Diamonds />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Beaming)}>
        <Svg_Emoji_Beaming />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Crying)}>
        <Svg_Emoji_Crying />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Jack)}>
        <Svg_Emoji_WinkingFace />
      </div>
      <div className="toolbar__button  p-[8%]" onClick={onSignalClick(PlayerSignal.Ten)}>
        <Svg_Emoji_Fist />
      </div>
      <div className="toolbar__button  p-[5%]" onClick={onSignalClick(PlayerSignal.ThumbsDown)}>
        <Svg_Emoji_ThumbsDown />
      </div>
      <div className="toolbar__button p-[5%]" onClick={onSignalClick(PlayerSignal.ThumbsUp)}>
        <Svg_Emoji_ThumbsUp />
      </div>
    </div>
  </div>
}
