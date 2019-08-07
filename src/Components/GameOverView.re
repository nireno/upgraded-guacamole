[@bs.val]
external allfours_feedback_url: Js.Nullable.t(string) = "process.env.allfours_feedback_url";

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
    {switch (Js.Nullable.toOption(allfours_feedback_url)) {
     | None => ReasonReact.null
     | Some(href) =>
       <div className="text-xs text-center mt-8">
         <span> {ReasonReact.string("Love it / Hate it: ")} </span>
         <a href target="_blank" rel="noopener noreferrer">
           {ReasonReact.string("click here to send feedback.")}
         </a>
       </div>
     }}
  </>;
};
