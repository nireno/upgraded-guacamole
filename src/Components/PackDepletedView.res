@react.component
let make = (~reshuffleClick) =>
  <div>
    <div> {React.string("No more cards")} </div>
    <button className="btn btn-blue" onClick=reshuffleClick> {React.string("Reshuffle")} </button>
  </div>
