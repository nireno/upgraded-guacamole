[@react.component]
let make = (~onSave, ~settings) => {
  let (allowSubbing, updateAllowSubbing) = React.useState(() => settings.ClientSettings.allowSubbing);
  let (volume, updateVolume) = React.useState(() => settings.ClientSettings.volume);

  let volumeLevel: float =
    switch (volume) {
    | Mute(v) => v
    | Level(v) => v
    };

  let handleVolumeChange = event => {
    let inputVal = event->ReactEvent.Form.target##value;
    let floatVal =
      try (float_of_string(inputVal)) {
      | _ => volumeLevel
      };
    let floatVal = floatVal < 0.0 ? 0.0 : floatVal > 1.0 ? 1.0 : floatVal;
    let volume' =
      switch (volume) {
      | Mute(_) => ClientSettings.Mute(floatVal)
      | Level(_) => Level(floatVal)
      };
    updateVolume(_ => volume');
  };

  let onSaveClick = _event => {
    onSave(ClientSettings.{allowSubbing, volume});
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
    <div className="mb-4"> {ReasonReact.string("Audio Settings")} </div>
    <div className="mb-4 flex flex-row h-8">
      <img src="./static/img/emoji_speaker.svg" style=ReactDOMRe.Style.make(~height="100%", ~width="auto", ()) />
      <input  id="volume" type_="range" min=0 max="1" value={Js.Float.toString(volumeLevel)} step=0.01 onChange=handleVolumeChange className="flex-grow ml-4"/>
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
