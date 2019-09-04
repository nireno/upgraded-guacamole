[@react.component]
let make = (~onSave, ~settings) => {
  let (volume, updateVolume) = React.useState(() => settings.ClientSettings.volume);
  let (client_id, updateClientId) = React.useState(() => settings.ClientSettings.client_id);
  let (client_profile_type, updateClientProfileType) =
    React.useState(() => settings.ClientSettings.client_profile_type);
  let (client_initials, updateClientInitials) =
    React.useState(() => settings.ClientSettings.client_initials);

  let blipSoundRef =
    React.useRef(Howler.(makeHowl(options(~src=[|"./static/audio/blip.mp3"|], ()))));

  let volumeLevel: float =
    switch (volume) {
    | Mute(_) => 0.0 
    | Level(v) => v
    };
  
  let handleVolumeChange = event => {
    let inputVal = event->ReactEvent.Form.target##value;
    let floatVal =
      try (float_of_string(inputVal)) {
      | _ => volumeLevel
      };
    let floatVal = floatVal < 0.0 ? 0.0 : floatVal > 1.0 ? 1.0 : floatVal;
    let volume' = floatVal == 0.0 ? ClientSettings.Mute(0.0) : Level(floatVal);
    updateVolume(_ => volume');
    let sound = React.Ref.current(blipSoundRef);
    Howler.volume(sound, floatVal);
    if (!Howler.playing(sound)) {
      Howler.play(sound);
    };
  };

  let volumeIcon = switch(volume){
  | Mute(_) => "emoji_speaker_muted"
  | Level(_) => "emoji_speaker"
  };

  let handleVolumeIconClick = _event => {
    let volume' = switch(volume){
    | Mute(n) when n == 0.0 => ClientSettings.defaults.volume
    | Mute(n) => ClientSettings.Level(n)
    | Level(n) => Mute(n)
    };
    updateVolume(_ => volume');
  };

  let onClientProfileTypeChanged = event => {
    let client_profile_type = switch(event->ReactEvent.Form.target##value){
    | "feminine" => ClientSettings.Feminine
    | "machine" => ClientSettings.Machine
    | "abstract" => ClientSettings.Abstract
    | _ => Masculine
    };

    updateClientProfileType(_ => client_profile_type);
  };

  let onChangeInitials = event => {
    let inputVal = event->ReactEvent.Form.target##value;
    let inputStripped = Js.String.split("", inputVal)
    ->Belt.Array.map(Js.String.toUpperCase)
    ->Belt.Array.keepMap(Js.String.match([%re "/[A-Za-z]/"]))
    ->Js.Array.joinWith("", _);
    updateClientInitials(_ => inputStripped);
  };

  let onChangeClientId = _event => {
    updateClientId(_ => Nanoid.nanoid());
  };

  let onSaveClick = _event => {
    onSave(ClientSettings.{volume, client_id, client_profile_type, client_initials});
    ReasonReactRouter.replace("./");
  };

  let dicebearType = ClientSettings.dicebearTypeOfProfileType(client_profile_type);
  
  <div className="bg-white shadow-md border border-solid border-gray-300 rounded px-8 pt-6 pb-8 mb-4 w-10/12">
    <div className="mb-4 text-xl text-center"> {ReasonReact.string("Settings")} </div>
    <div className="mb-4 text-lg"> {ReasonReact.string("Profile")} </div>
    <div className="flex">
      <img
        src={j|https://avatars.dicebear.com/v2/$dicebearType/$client_id.svg|j}
        className="rounded border border-gray-300 p-2 w-1/5 cursor-pointer"
        onClick=onChangeClientId
      />
      <div className="flex flex-col flex-grow ml-8">
        <div className="flex justify-between">
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-male"
              name="client-profile-type"
              value="masculine"
              defaultChecked={client_profile_type == Masculine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-male"> {ReasonReact.string("Masculine")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-female"
              name="client-profile-type"
              value="feminine"
              defaultChecked={client_profile_type == Feminine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-female"> {ReasonReact.string("Feminine")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-machine"
              name="client-profile-type"
              value="machine"
              defaultChecked={client_profile_type == Machine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-machine"> {ReasonReact.string("Machine")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-abstract"
              name="client-profile-type"
              value="abstract"
              defaultChecked={client_profile_type == Abstract}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-abstract"> {ReasonReact.string("Abstract")} </label>
          </div>
        </div>
        <div className="mt-4">
          <label htmlFor="client-initials"> {ReasonReact.string("Initials: ")} </label>
          <input
            style={ReactDOMRe.Style.make(~maxWidth="10em", ())}
            placeholder="Enter your initials"
            id="client-initials"
            onChange=onChangeInitials
            className="focus:outline-0 border border-transparent focus:bg-white focus:border-gray-300
                       placeholder-gray-600 rounded-lg bg-gray-200 p-2 ml-4 appearance-none leading-normal
                       text-center"
            type_="text"
            maxLength=2
            pattern="[A-Za-z]*"
            value=client_initials
          />
        </div>
      </div>
    </div>
    <form className="mt-8">
      <div className="mb-4 text-lg"> {ReasonReact.string("Sound Effects Volume")} </div>
      <div className="mb-4 flex flex-row h-8">
        <div
          className="bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold rounded inline-flex items-center cursor-pointer"
          onClick=handleVolumeIconClick>
          <img
            src={j|./static/img/$volumeIcon.svg|j}
            className="block mx-4"
            style={ReactDOMRe.Style.make(
              ~height="100%",
              ~width="auto",
              ~minHeight="32px",
              ~minWidth="32px",
              (),
            )}
          />
        </div>
        <input
          id="volume"
          type_="range"
          min=0
          max="1"
          value={Js.Float.toString(volumeLevel)}
          step=0.05
          onChange=handleVolumeChange
          className="flex-grow ml-4 cursor-pointer"
        />
      </div>
      <div className="flex items-center justify-around">
        <div
          onClick={_ => ReasonReactRouter.replace("./")}
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
    </form>
  </div>;
};
