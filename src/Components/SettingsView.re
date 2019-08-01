[@react.component]
let make = (~onSave, ~settings) => {
  let (volume, updateVolume) = React.useState(() => settings.ClientSettings.volume);
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
    let volume' =
      switch (volume) {
      | Mute(_) // Modifying the volume should unmute sound
      | Level(_) => ClientSettings.Level(floatVal)
      };
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
    | Mute(n) => ClientSettings.Level(n)
    | Level(n) => Mute(n)
    };
    updateVolume(_ => volume');
  };

  let onSaveClick = _event => {
    onSave(ClientSettings.{volume: volume});
    ReasonReactRouter.push("./");
  };
  
  <form className="bg-white shadow-md border border-solid border-gray-300 rounded px-8 pt-6 pb-8 mb-4 w-10/12">
    <div className="mb-4 text-xl text-center"> {ReasonReact.string("All Fours Settings")} </div>
    <div className="mb-4"> {ReasonReact.string("Sound Effects Volume")} </div>
    <div className="mb-4 flex flex-row h-8">
      <img
        src={j|./static/img/$volumeIcon.svg|j}
        onClick=handleVolumeIconClick
        className="cursor-pointer"
        style={ReactDOMRe.Style.make(~height="100%", ~width="auto", ())}
      />
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
