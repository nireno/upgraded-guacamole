[@react.component]
let make = (~onSave, ~settings) => {
  let (allowSubbing, updateAllowSubbing) = React.useState(() => settings.ClientSettings.allowSubbing);

  let onSaveClick = _event => {
    onSave(ClientSettings.{allowSubbing: allowSubbing});
    ReasonReactRouter.push("/");
  };

  <form className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4">
    <div className="font-bold mb-4"> {ReasonReact.string("Joining a game")} </div>
    <div className="mb-4 flex flex-row ">
      <input defaultChecked={allowSubbing ? true : false} type_="radio" name="substitute" id="substitute-yes" onClick={_ => updateAllowSubbing(_prev => true)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-yes">
        {ReasonReact.string("Shorter wait times (occasionally join as a substitute player)")}
      </label>
    </div>
    <div className="mb-4 flex flex-row ">
      <input type_="radio" name="substitute" id="substitute-no" defaultChecked={allowSubbing ? false : true} onClick={_ => updateAllowSubbing(_prev => false)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-no">
        {ReasonReact.string("Normal wait times (never join as a substitute player)")}
      </label>
    </div>
    <div className="flex items-center justify-around">
      <a
        onClick={_ => ReasonReactRouter.push("/")}
        className="inline-block align-baseline font-bold text-sm text-blue-500 hover:text-blue-800"
        href="#">
        {ReasonReact.string("Cancel")}
      </a>
      <button
        onClick=onSaveClick
        className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
        type_="button">
        {ReasonReact.string("Save")}
      </button>
    </div>
  </form>;
};
