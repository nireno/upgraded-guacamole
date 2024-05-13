@react.component
let make = (~onToggleSortClick, ~onSignalClick, ~sortHand) => {
  let src = sortHand ? "./static/img/sorted.svg" : "./static/img/unsorted.svg"
  <div className="toolbar-layers relative m-1">
    <div className="layer bg-white opacity-25" />
    <div className="toolbar relative grid grid-cols-12 gap-1 z-10 p-1">
      <img
        className="toolbar__button bg-white"
        onClick=onToggleSortClick
        src
        style={ReactDOM.Style.make(~height="auto", ())}
      />
      <div className="placeholder" />
      <ToolbarSignal onClick={onSignalClick(PlayerSignal.Spades)} img="./static/img/spade.svg" />
      <ToolbarSignal onClick={onSignalClick(PlayerSignal.Hearts)} img="./static/img/heart.svg" />
      <ToolbarSignal onClick={onSignalClick(PlayerSignal.Clubs)} img="./static/img/club.svg" />
      <ToolbarSignal
        onClick={onSignalClick(PlayerSignal.Diamonds)} img="./static/img/diamond.svg"
      />
      <ToolbarSignal
        onClick={onSignalClick(PlayerSignal.Beaming)} img="./static/img/emoji_beaming.svg"
      />
      <ToolbarSignal
        onClick={onSignalClick(PlayerSignal.Crying)} img="./static/img/emoji_crying.svg"
      />
      <ToolbarSignal
        onClick={onSignalClick(PlayerSignal.Jack)} img="./static/img/emoji_winking_face.svg"
      />
      <ToolbarSignal onClick={onSignalClick(PlayerSignal.Ten)} img="./static/img/emoji_fist.svg" />
      <img
        className="toolbar__button  p-[5%]"
        onClick={onSignalClick(PlayerSignal.ThumbsDown)}
        src="./static/img/emoji_thumbs_down.svg"
      />
      <img
        className="toolbar__button p-[5%]"
        onClick={onSignalClick(PlayerSignal.ThumbsUp)}
        src="./static/img/emoji_thumbs_up.svg"
      />
    </div>
  </div>
}
