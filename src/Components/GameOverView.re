[@react.component]
let make = (~weScore, ~demScore, ~decisiveAward as maybeDecisiveAward=?, ~playAgainClick, ~leaveClick) => {
  let decisionText =
    switch (maybeDecisiveAward) {
    | None => ""
    | Some(decisiveAward) =>
      switch (decisiveAward) {
      | GameAward.KickDecides(card) =>
        "Final blow from kicking " ++ card->Card.stringOfCard
      | GameAward.HighDecides(card) => "Winner has " ++ card->Card.stringOfCard ++ "for high."
      | LowDecides(card) => "Winner has " ++ card->Card.stringOfCard ++ "for low."
      | RunJackDecides => "Winner gets away with jack"
      | HangJackDecides => "Winner hangs the jack"
      | HighAndLowDecides(highCard, lowCard) =>
        "Winner has "
        ++ highCard->Card.stringOfCard
        ++ " for high and "
        ++ lowCard->Card.stringOfCard
        ++ " for low."
      | HighAndRunJackDecides(highCard) =>
        "Winner has "
        ++ highCard->Card.stringOfCard
        ++ " for high and gets away with the Jack"
      | HighAndHangJackDecides(highCard) =>
        "Winner has "
        ++ highCard->Card.stringOfCard
        ++ " for high and hangs the Jack"
      | LowAndRunJackDecides(lowCard) =>
        "Winner has "
        ++ lowCard->Card.stringOfCard
        ++ " for low and gets away with the Jack"
      | LowAndHangJackDecides(lowCard) =>
        "Winner has "
        ++ lowCard->Card.stringOfCard
        ++ " for low and hangs the Jack"
      | HighLowAndRunJackDecides(highCard, lowCard) =>
        "Winner has "
        ++ highCard->Card.stringOfCard
        ++ " for high, "
        ++ lowCard->Card.stringOfCard
        ++ " for low and gets away with the Jack"
      | HighLowAndHangJackDecides(highCard, lowCard) =>
        "Winner has "
        ++ highCard->Card.stringOfCard
        ++ " for high, "
        ++ lowCard->Card.stringOfCard
        ++ " for low and hangs the Jack"
      }
    };
  let (outcomeText, outcomeImg) =
    weScore >= demScore
      ? ("We win!", "./static/img/emoji_beaming.svg")
      : ("We lost...", "./static/img/emoji_crying.svg");
  <>
    <div> {outcomeText |> ReasonReact.string} </div>
    <div> {decisionText |> ReasonReact.string} </div>
    <img src=outcomeImg style={ReactDOMRe.Style.make(~width="15%", ())} />
    <div className="flex flex-row justify-around w-full">
      <button className="btn btn-grey" onClick=leaveClick>
        {ReasonReact.string("Back Home")}
      </button>
      <button className="btn btn-blue" onClick=playAgainClick>
        {ReasonReact.string("Play Again")}
      </button>
    </div>
  </>;
};
