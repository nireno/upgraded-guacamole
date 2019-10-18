[@react.component]
let make = (~onToggleSortClick, ~sortHand) => {
  let src = sortHand ? "./static/img/sorted.svg" : "./static/img/unsorted.svg";
  <div className="toolbar-layers flex-grow relative m-1">
    <div className="layer bg-white opacity-25"></div>
    <div className="toolbar relative z-10 p-1">
      <img className="toolbar__button bg-white" onClick=onToggleSortClick src style={ReactDOMRe.Style.make(~width="6%", ())} />
    </div>
  </div>;
};