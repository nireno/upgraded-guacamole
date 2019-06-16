[@react.component]
let make = (~weTeamId, ~maybeTeamHigh, ~maybeTeamLow, ~maybeTeamJack, ~maybeTeamGame) => {
  <table className="round-summary-table w-full">
    <thead>
      <tr>
        <th className="w-1/2"> {ReasonReact.string("We")} </th>
        <th> {ReasonReact.string("Dem")} </th>
      </tr>
    </thead>
    <tbody>
      <tr>
        {switch (maybeTeamHigh) {
         | None => ReasonReact.null
         | Some(teamId) =>
           teamId == weTeamId
             ? <>
                 <td>
                   {ReasonReact.string(
                      "High (+" ++ (GameAward.value(GameAward.HighAward) |> string_of_int) ++ ")",
                    )}
                 </td>
                 <td> {ReasonReact.string("-")} </td>
               </>
             : <>
                 <td> {ReasonReact.string("-")} </td>
                 <td>
                   {ReasonReact.string(
                      "High (+" ++ (GameAward.value(GameAward.HighAward) |> string_of_int) ++ ")",
                    )}
                 </td>
               </>
         }}
      </tr>
      <tr>
        {switch (maybeTeamLow) {
         | None => ReasonReact.null
         | Some(teamId) =>
           teamId == weTeamId
             ? <>
                 <td>
                   {ReasonReact.string(
                      "Low (+" ++ (GameAward.value(GameAward.LowAward) |> string_of_int) ++ ")",
                    )}
                 </td>
                 <td> {ReasonReact.string("-")} </td>
               </>
             : <>
                 <td> {ReasonReact.string("-")} </td>
                 <td>
                   {ReasonReact.string(
                      "Low (+" ++ (GameAward.value(GameAward.LowAward) |> string_of_int) ++ ")",
                    )}
                 </td>
               </>
         }}
      </tr>
      <tr>
        {switch (maybeTeamJack) {
         | None => ReasonReact.null
         | Some((teamId, jackAward)) =>
           teamId == weTeamId
             ? <>
                 <td>
                   {ReasonReact.string(
                      GameAward.toString(jackAward)
                      ++ " (+"
                      ++ (GameAward.value(jackAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
                 <td> {ReasonReact.string("-")} </td>
               </>
             : <>
                 <td> {ReasonReact.string("-")} </td>
                 <td>
                   {ReasonReact.string(
                      GameAward.toString(jackAward)
                      ++ " (+"
                      ++ (GameAward.value(jackAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
               </>
         }}
      </tr>
      <tr>
        {switch (maybeTeamGame) {
         | None => <> <td colSpan=2> {ReasonReact.string("Tied for game.")} </td> </>
         | Some(teamId) =>
           teamId == weTeamId
             ? <>
                 <td>
                   {ReasonReact.string(
                      GameAward.toString(GameAward.GameAward)
                      ++ " (+"
                      ++ (GameAward.value(GameAward.GameAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
                 <td> {ReasonReact.string("-")} </td>
               </>
             : <>
                 <td> {ReasonReact.string("-")} </td>
                 <td>
                   {ReasonReact.string(
                      GameAward.toString(GameAward.GameAward)
                      ++ " (+"
                      ++ (GameAward.value(GameAward.GameAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
               </>
         }}
      </tr>
    </tbody>
  </table>;
};
