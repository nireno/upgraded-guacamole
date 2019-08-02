[@react.component]
let make = (~onLeaveClick, ~inviteCode, ~n) => {
  let initialCopyText = "copy invite code";
  let (copyText, updateCopyText) = React.useState(() => initialCopyText);
  let friends = Grammar.byNumber(n, "friend");
  let n = string_of_int(n);

  let onCopySuccess = _event => {
    updateCopyText(_ => "Copied!");
    Js.Global.setTimeout(() => updateCopyText(_ => initialCopyText), 2000) |> ignore;
  };

  let onCopyFailure = _event => {
    updateCopyText(_ => "Sorry. Please manually copy the invite code.");
  };

  React.useEffect0(() => {
    let stringOfTrigger = _ => inviteCode;
    let clipboard =
      Clipboard.make(".copy-invite-code", Clipboard.options(~text=stringOfTrigger));
    clipboard->Clipboard.on("success", onCopySuccess);
    clipboard->Clipboard.on("error", onCopyFailure);
    None;
  });

  <div className="text-center">
    <div> {ReasonReact.string({j|Invite friends|j})} </div>
    <div className="text-xl">
      {ReasonReact.string({j|Tell your friends to join a private game with this invite code:|j})}
    </div>
    <div className="mt-6 mb-1"> {ReasonReact.string({j|$inviteCode|j})} </div>
    <a
      className="copy-invite-code block text-base text-blue-700 hover:text-blue-500 underline cursor-pointer">
      {ReasonReact.string(copyText)}
    </a>
    <div className="text-xl mt-6">
      {ReasonReact.string({j|Waiting for $n more $friends...|j})}
    </div>
    <button className="btn btn-blue mt-4" onClick=onLeaveClick>
      {ReasonReact.string("Cancel")}
    </button>
  </div>;
};
