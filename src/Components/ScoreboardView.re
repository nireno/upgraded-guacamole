[@react.component]
let make = (~weScore, ~wePoints, ~demScore, ~demPoints) =>
  <div className="score-board flex flex-row justify-around bg-gray-900 text-white py-4 -z-20">
    <div className="text-center">
      <div> {ReasonReact.string("We")} </div>
      <div className="text-lg"> {ReasonReact.string(string_of_int(weScore))} </div>
      <div className="text-sm text-gray-600"> {ReasonReact.string(string_of_int(wePoints))} </div>
    </div>
    <div className="text-center">
      {ReasonReact.string("Score")}
    </div>
    <div className="text-center">
      <div> {ReasonReact.string("Dem")} </div>
      <div className="text-lg"> {ReasonReact.string(string_of_int(demScore))} </div>
      <div className="text-sm text-gray-600"> {ReasonReact.string(string_of_int(demPoints))} </div>
    </div>
  </div>
