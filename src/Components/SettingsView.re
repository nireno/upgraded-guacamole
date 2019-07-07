[@react.component]
let make = (~onSave, ~settings) => {
  let (allowSubbing, updateAllowSubbing) = React.useState(() => settings.ClientSettings.allowSubbing);

  let onSaveClick = _event => {
    onSave(ClientSettings.{allowSubbing: allowSubbing});
    ReasonReactRouter.push("./");
  };

  <form className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4">
    <div className="mb-4 text-xl text-center"> {ReasonReact.string("All Fours Settings")} </div>
    <div className="mb-4"> {ReasonReact.string("Joining a game")} </div>
    <div className="mb-4 flex flex-row ">
      <input defaultChecked={allowSubbing ? true : false} type_="radio" name="substitute" id="substitute-yes" onClick={_ => updateAllowSubbing(_prev => true)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-yes">
        {ReasonReact.string("Shorter wait times (allow joining games already in progress)")}
      </label>
    </div>
    <div className="mb-4 flex flex-row ">
      <input type_="radio" name="substitute" id="substitute-no" defaultChecked={allowSubbing ? false : true} onClick={_ => updateAllowSubbing(_prev => false)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-no">
        {ReasonReact.string("Normal wait times (only join new games)")}
      </label>
    </div>
    <div className="flex items-center justify-around">
      <div
        onClick={_ => ReasonReactRouter.push("./")}
        className="link link-blue"
        href="#">
        {ReasonReact.string("Cancel")}
      </div>
      <button
        onClick=onSaveClick
        className="btn btn-blue"
        type_="button">
        {ReasonReact.string("Save")}
      </button>
    </div>
  </form>;
};
