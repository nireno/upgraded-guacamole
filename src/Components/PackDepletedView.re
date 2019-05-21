[@react.component]
let make = (~reshuffleClick) =>
  <div>
    <div> {ReasonReact.string("No more cards")} </div>
    <button className="btn btn-blue" onClick=reshuffleClick>
       {ReasonReact.string("Reshuffle")} </button>
  </div>;
