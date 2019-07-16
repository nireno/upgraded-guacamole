[@react.component]
let make = (~inviteCode, ~n ) => {
  let friends = Grammar.byNumber(n, "friend");
  let n = string_of_int(n);
  <div className="text-center">
    <div className="text-xl">{ReasonReact.string({j|Here's your invite code|j})}</div>
    <div className="my-6">{ReasonReact.string({j|$inviteCode|j})}</div>
    <div className="text-xl">{ReasonReact.string({j|Waiting for $n more $friends...|j})}</div>
  </div>;
};
