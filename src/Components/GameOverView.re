[@react.component]
let make = (~weScore, ~demScore, ~playAgainClick, ~leaveClick) => {
  let (outcomeText, outcomeImg) = weScore >= demScore ? ("We win!", "./static/img/emoji_beaming.svg") : ("We lost...", "./static/img/emoji_crying.svg");
  <>
    <div> {outcomeText |> ReasonReact.string} </div>
    <img src=outcomeImg style=ReactDOMRe.Style.make(~width="25%",())/>
    <button className="btn btn-blue" onClick=playAgainClick>{ ReasonReact.string("Play Again")}</button>
    <button className="btn btn-grey mt-4" onClick=leaveClick>{ ReasonReact.string("Back Home") }</button>
  </>;
};
