[@react.component]
let make = (~wePoints, ~demPoints) => {
  let outcome = wePoints >= demPoints ? "We win :)" : "We loss :(";
  <>
    <h1> {ReasonReact.string("Game over")} </h1>
    <div> {outcome |> ReasonReact.string} </div>
  </>;
};
