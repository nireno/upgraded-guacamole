[@react.component]
let make = (~weScore, ~demScore, ~playAgainClick, ~leaveClick) => {
  let outcome = weScore >= demScore ? "We win :)" : "We lost :(";
  <>
    <h1> {ReasonReact.string("Game over")} </h1>
    <div> {outcome |> ReasonReact.string} </div>
    <button className="btn btn-blue" onClick=playAgainClick>{ ReasonReact.string("Play Again") }</button>
    <button className="btn btn-grey" onClick=leaveClick>{ ReasonReact.string("Quit") }</button>
  </>;
};
