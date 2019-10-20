[@react.component]
let make = (~onToggleSortClick, ~onSignalClick, ~sortHand) => {
  let src = sortHand ? "./static/img/sorted.svg" : "./static/img/unsorted.svg";
  <div className="toolbar-layers flex-grow relative m-1">
    <div className="layer bg-white opacity-25"></div>
    <div className="toolbar relative flex flex-row justify-between items-center z-10 p-1">
      <div className="toolbar__section" style=ReactDOMRe.Style.make(~width="7%", ())>
        <img className="toolbar__button bg-white" onClick=onToggleSortClick src style=ReactDOMRe.Style.make(~height="auto", ())/>
      </div>
      <div className="toolbar__section toolbar_section--signals flex-grow flex flex-row justify-end">
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Spades)} img="./static/img/spade.svg" />
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Hearts)} img="./static/img/heart.svg" />
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Clubs)} img="./static/img/club.svg" />
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Diamonds)} img="./static/img/diamond.svg" />
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Beaming)} img="./static/img/emoji_beaming.svg" />
        <ToolbarSignal onClick={onSignalClick(PlayerSignal.Crying)} img="./static/img/emoji_crying.svg" />
        <img
          className="toolbar__button"
          onClick=onSignalClick(PlayerSignal.ThumbsUp)
          src="./static/img/emoji_thumbs_up.svg"
          style={ReactDOMRe.Style.make(~width="8%", ~height="auto", ())}
        />
        <img
          className="toolbar__button"
          onClick=onSignalClick(PlayerSignal.ThumbsDown)
          src="./static/img/emoji_thumbs_down.svg"
          style={ReactDOMRe.Style.make(~width="8%", ~height="auto", ())}
        />
      </div>
    </div>
  </div>;
};