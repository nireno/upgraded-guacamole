@react.component
let make = (~weScore, ~wePoints, ~demScore, ~demPoints) =>
  <div className="score-board flex flex-row justify-around bg-gray-900 text-white py-4 -z-20">
    <div className="text-center">
      <div> {React.string("We")} </div>
      <div className="text-lg"> {React.string(string_of_int(weScore))} </div>
      <div className="text-sm text-gray-600"> {React.string(string_of_int(wePoints))} </div>
    </div>
    <div className="text-center"> {React.string("Score")} </div>
    <div className="text-center">
      <div> {React.string("Dem")} </div>
      <div className="text-lg"> {React.string(string_of_int(demScore))} </div>
      <div className="text-sm text-gray-600"> {React.string(string_of_int(demPoints))} </div>
    </div>
  </div>
