[@react.component]
let make = (~weScore, ~demScore, ~playAgainClick, ~leaveClick) => {
  let (outcomeText, outcomeImg) =
    weScore >= demScore
      ? ("We win!", "./static/img/emoji_beaming.svg")
      : ("We lost...", "./static/img/emoji_crying.svg");
  <>
    <div> {outcomeText |> ReasonReact.string} </div>
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
