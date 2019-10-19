[@react.component]
let make = (~onToggleSortClick, ~onSignalClick, ~sortHand) => {
  let src = sortHand ? "./static/img/sorted.svg" : "./static/img/unsorted.svg";
  <div className="toolbar-layers flex-grow relative m-1">
    <div className="layer bg-white opacity-25"></div>
    <div className="toolbar relative flex flex-row justify-between z-10 p-1">
      <div className="toolbar__section" style=ReactDOMRe.Style.make(~width="9%", ())>
        <img className="toolbar__button bg-white" onClick=onToggleSortClick src style=ReactDOMRe.Style.make(~height="auto", ())/>
      </div>
      <div className="toolbar__section toolbar_section--signals flex-grow flex flex-row justify-end">
        <img
          className="toolbar__button"
          onClick=onSignalClick(PlayerSignal.ThumbsUp)
          src="./static/img/emoji_thumbs_up.svg"
          style={ReactDOMRe.Style.make(~width="10%", ~height="auto", ())}
        />
        <img
          className="toolbar__button"
          onClick=onSignalClick(PlayerSignal.ThumbsDown)
          src="./static/img/emoji_thumbs_down.svg"
          style={ReactDOMRe.Style.make(~width="10%", ~height="auto", ())}
        />
      </div>
    </div>
  </div>;
};