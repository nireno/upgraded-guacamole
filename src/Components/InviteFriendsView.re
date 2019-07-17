[@react.component]
let make = (~inviteCode, ~n) => {
  let initialCopyText = "copy invite code"
  let (copyText, updateCopyText) = React.useState(() => initialCopyText);
  let friends = Grammar.byNumber(n, "friend");
  let n = string_of_int(n);

  let onCopyClick = _event => {
    updateCopyText(_ => "Copied!");
    Js.Global.setTimeout(() => updateCopyText(_ => initialCopyText), 2000) |> ignore;
    Clipboard.copy(inviteCode);
  };

  <div className="text-center">
    <div> {ReasonReact.string({j|Invite friends|j})} </div>
    <div className="text-xl">
      {ReasonReact.string({j|Tell your friends to join a private game with this invite code:|j})}
    </div>
    <div className="mt-6 mb-1"> {ReasonReact.string({j|$inviteCode|j})} </div>
    <a className="block text-base text-blue-700 hover:text-blue-500 underline cursor-pointer" onClick=onCopyClick>
      {ReasonReact.string(copyText)}
    </a>
    <div className="text-xl mt-6">
      {ReasonReact.string({j|Waiting for $n more $friends...|j})}
    </div>
  </div>;
};
