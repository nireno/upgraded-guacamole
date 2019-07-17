[@react.component]
let make = (~inviteCode, ~n ) => {
  let friends = Grammar.byNumber(n, "friend");
  let n = string_of_int(n);
  <div className="text-center">
    <div>{ReasonReact.string({j|Invite friends|j})}</div>
    <div className="text-xl">{ReasonReact.string({j|Tell your friends to join a private game with this invite code:|j})}</div>
    <div className="my-6">{ReasonReact.string({j|$inviteCode|j})}</div>
    <div className="text-xl">{ReasonReact.string({j|Waiting for $n more $friends...|j})}</div>
  </div>;
};
