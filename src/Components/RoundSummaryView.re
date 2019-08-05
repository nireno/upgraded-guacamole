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
         | Some((teamId, _card)) =>
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
         | Some((teamId, _card)) =>
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
                      GameAward.stringOfJackAward(jackAward)
                      ++ " (+"
                      ++ (GameAward.jackAwardValue(jackAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
                 <td> {ReasonReact.string("-")} </td>
               </>
             : <>
                 <td> {ReasonReact.string("-")} </td>
                 <td>
                   {ReasonReact.string(
                      GameAward.stringOfJackAward(jackAward)
                      ++ " (+"
                      ++ (GameAward.jackAwardValue(jackAward) |> string_of_int)
                      ++ ")",
                    )}
                 </td>
               </>
         }}
      </tr>
      <tr>
        {
          switch (maybeTeamGame) {
          | None => <> <td colSpan=2> {ReasonReact.string("Tied for game.")} </td> </>
          | Some((teamId, winnerPoints, loserPoints)) =>
            teamId == weTeamId
              ? <>
                  <td>
                    {ReasonReact.string(
                       winnerPoints->string_of_int
                       ++ " for "
                       ++ GameAward.toString(GameAward.GameAward)
                       ++ " (+"
                       ++ (GameAward.value(GameAward.GameAward) |> string_of_int)
                       ++ ")",
                     )}
                  </td>
                  <td>
                    {ReasonReact.string(
                       loserPoints->string_of_int ++ " for " ++ GameAward.toString(GameAward.GameAward),
                     )}
                  </td>
                </>
              : <>
                  <td>
                    {ReasonReact.string(
                       loserPoints->string_of_int ++ " for " ++ GameAward.toString(GameAward.GameAward),
                     )}
                  </td>
                  <td>
                    {ReasonReact.string(
                       winnerPoints->string_of_int
                       ++ " for "
                       ++ GameAward.toString(GameAward.GameAward)
                       ++ " (+"
                       ++ (GameAward.value(GameAward.GameAward) |> string_of_int)
                       ++ ")",
                     )}
                  </td>
                </>
          };
        }
      </tr>
    </tbody>
  </table>;
};
