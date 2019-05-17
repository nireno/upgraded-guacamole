[@react.component]
let make = (~wePoints, ~demPoints) =>
  <div className="scoreboard flex flex-row justify-around">
    <div className="text-center">
      <div> {ReasonReact.string("We")} </div>
      <div> {ReasonReact.string(string_of_int(wePoints))} </div>
    </div>
    <div className="text-center">
      {ReasonReact.string("Score")}
    </div>
    <div className="text-center">
      <div> {ReasonReact.string("Dem")} </div>
      <div> {ReasonReact.string(string_of_int(demPoints))} </div>
    </div>
  </div>;
