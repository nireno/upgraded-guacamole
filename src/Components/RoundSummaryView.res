@react.component
let make = (~weTeamId, ~maybeTeamHigh, ~maybeTeamLow, ~maybeTeamJack, ~maybeTeamGame) =>
  <table className="round-summary-table w-full">
    <thead>
      <tr>
        <th className="w-1/2"> {React.string("We")} </th>
        <th> {React.string("Dem")} </th>
      </tr>
    </thead>
    <tbody>
      <tr>
        {switch maybeTeamHigh {
        | None => ReasonReact.null
        | Some({GameAward.team_id: team_id, winning_card, losing_card_maybe}) =>
          team_id == weTeamId
            ? <>
                <td>
                  {React.string(
                    winning_card.rank->Card.Rank.stringOfRank ++
                      (" for high (+" ++
                      ((Ruleset.default.highAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
                {switch losing_card_maybe {
                | None => <td> {React.string("-")} </td>
                | Some(losing_card) =>
                  <td>
                    {React.string(losing_card.rank->Card.Rank.stringOfRank ++ " for high")}
                  </td>
                }}
              </>
            : <>
                {switch losing_card_maybe {
                | None => <td> {React.string("-")} </td>
                | Some(losing_card) =>
                  <td>
                    {React.string(losing_card.rank->Card.Rank.stringOfRank ++ " for high")}
                  </td>
                }}
                <td>
                  {React.string(
                    winning_card.rank->Card.Rank.stringOfRank ++
                      (" for high (+" ++
                      ((Ruleset.default.highAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
              </>
        }}
      </tr>
      <tr>
        {switch maybeTeamLow {
        | None => ReasonReact.null
        | Some({GameAward.team_id: team_id, winning_card, losing_card_maybe}) =>
          team_id == weTeamId
            ? <>
                <td>
                  {React.string(
                    winning_card.rank->Card.Rank.stringOfRank ++
                      (" for low (+" ++
                      ((Ruleset.default.lowAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
                {switch losing_card_maybe {
                | None => <td> {React.string("-")} </td>
                | Some(losing_card) =>
                  <td>
                    {React.string(losing_card.rank->Card.Rank.stringOfRank ++ " for low")}
                  </td>
                }}
              </>
            : <>
                {switch losing_card_maybe {
                | None => <td> {React.string("-")} </td>
                | Some(losing_card) =>
                  <td>
                    {React.string(losing_card.rank->Card.Rank.stringOfRank ++ " for low")}
                  </td>
                }}
                <td>
                  {React.string(
                    winning_card.rank->Card.Rank.stringOfRank ++
                      (" for low (+" ++
                      ((Ruleset.default.lowAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
              </>
        }}
      </tr>
      <tr>
        {switch maybeTeamJack {
        | None => ReasonReact.null
        | Some({GameAward.team_id: team_id, jack_award_type}) =>
          team_id == weTeamId
            ? <>
                <td>
                  {React.string(
                    GameAward.stringOfJackAward(jack_award_type) ++
                    (" (+" ++
                    ((GameAward.jackAwardValue(jack_award_type) |> string_of_int) ++ ")")),
                  )}
                </td>
                <td> {React.string("-")} </td>
              </>
            : <>
                <td> {React.string("-")} </td>
                <td>
                  {React.string(
                    GameAward.stringOfJackAward(jack_award_type) ++
                    (" (+" ++
                    ((GameAward.jackAwardValue(jack_award_type) |> string_of_int) ++ ")")),
                  )}
                </td>
              </>
        }}
      </tr>
      <tr>
        {switch maybeTeamGame {
        | None => ReasonReact.null
        | Some({GameAward.team_id_maybe: Some(T1 as team_id), winning_count, losing_count})
        | Some({GameAward.team_id_maybe: Some(T2 as team_id), winning_count, losing_count}) =>
          team_id == weTeamId
            ? <>
                <td>
                  {React.string(
                    winning_count->string_of_int ++
                      (" for game (+" ++
                      ((Ruleset.default.gameAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
                <td> {React.string(losing_count->string_of_int ++ " for game")} </td>
              </>
            : <>
                <td> {React.string(losing_count->string_of_int ++ " for game")} </td>
                <td>
                  {React.string(
                    winning_count->string_of_int ++
                      (" for game (+" ++
                      ((Ruleset.default.gameAwardValue |> string_of_int) ++ ")")),
                  )}
                </td>
              </>
        | Some(_gameAwardData) => <> <td colSpan=2> {React.string("Tied for game.")} </td> </>
        }}
      </tr>
    </tbody>
  </table>
