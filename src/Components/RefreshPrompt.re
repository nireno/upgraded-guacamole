[@react.component]
let make = () => {
  <>
    <div className="text-center">
      <div className="text-xl"> {ReasonReact.string("A new version is available")} </div>
      <div>
        {ReasonReact.string(
           "Please refresh your browser or tap the reload button below to get the latest version of All Fours.",
         )}
      </div>
    </div>
    <button onClick={_event => Window.reload(true)} className="btn btn-blue mt-4">
      {ReasonReact.string("Reload")}
    </button>
  </>;
};