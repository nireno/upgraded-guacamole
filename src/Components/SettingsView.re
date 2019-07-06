[@react.component]
let make = (~onSave, ~settings) => {
  let (substitution, updateSubstitution) = React.useState(() => settings.ClientSettings.substitution);

  let onSaveClick = _event => {
    onSave(ClientSettings.{substitution: substitution});
    ReasonReactRouter.push("/");
  };

  <form className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4">
    <div className="font-bold mb-4"> {ReasonReact.string("Joining a game")} </div>
    <div className="mb-4 flex flex-row ">
      <input defaultChecked={substitution ? true : false} type_="radio" name="substitute" id="substitute-yes" onClick={_ => updateSubstitution(_prev => true)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-yes">
        {ReasonReact.string("Shorter wait times (occasionally act as a substitute player)")}
      </label>
    </div>
    <div className="mb-4 flex flex-row ">
      <input type_="radio" name="substitute" id="substitute-no" defaultChecked={substitution ? false : true} onClick={_ => updateSubstitution(_prev => false)}/>
      <label className="text-gray-700 text-sm mb-2 ml-2" htmlFor="substitute-no">
        {ReasonReact.string("Normal wait times (never act as a substitute player)")}
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
