[@bs.val]
external allfours_feedback_url: Js.Nullable.t(string) = "process.env.allfours_feedback_url";

[@react.component]
let make = (~onJoinClick, ~canJoinPublicGame, ~onCancelClick) => {
  <div
    className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4">
    <div className="mb-4 text-xl text-center"> {ReasonReact.string("Here be dragons")} </div>
    <p className="mb-4">
      {ReasonReact.string(
         "Please note that All Fours Online is still in an experimental stage. This means that the game may go offline unexpectedly as we are making changes to the site.",
       )}
    </p>
    <p> {ReasonReact.string("Thank you for still giving it a shot.")} </p>
    {switch (Js.Nullable.toOption(allfours_feedback_url)) {
     | None => ReasonReact.null
     | Some(href) =>
       <p>
         <span>
           {ReasonReact.string("If you enjoy it, or have any comments/suggestions, ")}
         </span>
         <a href> {ReasonReact.string("click here to send feedback.")} </a>
       </p>
     }}
    <div className="flex items-center justify-around">
      <div onClick=onCancelClick className="link link-blue" href="#">
        {ReasonReact.string("Cancel")}
      </div>
      <button
        style={ReactDOMRe.Style.make(~minWidth="7rem", ())}
        onClick=onJoinClick
        className={"btn btn-blue " ++ (canJoinPublicGame ? "" : "btn-disabled")}
        disabled={!canJoinPublicGame}
        type_="button">
        {ReasonReact.string(canJoinPublicGame ? "Join Game" : "Joining")}
      </button>
    </div>
  </div>;
};
