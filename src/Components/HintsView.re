let changeIdenticon =
  <>
    <span> {ReasonReact.string("On the ")} </span>
    <span className="hint-menu-item-accent"> {ReasonReact.string("Settings")} </span>
    <span> {ReasonReact.string(" page of the main menu, you can change your profile icon by tapping on it.")} </span>
  </>;

let randomizedHints = My.List.shuffle(
  [ changeIdenticon
  ]);


[@react.component]
let make = () => {
  let hint = randomizedHints->List.hd;
  <div className="p-2 border rounded text-center bg-gray-200 text-gray-700">
    <div className="text-sm"> 
      <span>{ReasonReact.string("Hint: ")}</span>
      <span className="text-xs"> hint </span>
    </div>
  </div>
};