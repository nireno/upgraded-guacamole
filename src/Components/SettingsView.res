@react.component
let make = (~onSave, ~settings) => {
  let (volume, updateVolume) = React.useState(() => settings.ClientSettings.volume)
  let (client_id, updateClientId) = React.useState(() => settings.ClientSettings.client_id)
  let (client_profile_type, updateClientProfileType) = React.useState(() =>
    settings.ClientSettings.client_profile_type
  )
  let (client_initials, updateClientInitials) = React.useState(() =>
    settings.ClientSettings.client_initials
  )

  let blipSoundRef = React.useRef({
    open Howler
    makeHowl(options(~src=["./static/audio/blip.mp3"], ()))
  })

  let volumeLevel: float = switch volume {
  | Mute(_) => 0.0
  | Level(v) => v
  }

  let handleVolumeChange = event => {
    let inputVal = (event->ReactEvent.Form.target)["value"]
    let floatVal = try float_of_string(inputVal) catch {
    | _ => volumeLevel
    }
    let floatVal = floatVal < 0.0 ? 0.0 : floatVal > 1.0 ? 1.0 : floatVal
    let volume' = floatVal == 0.0 ? ClientSettings.Mute(0.0) : Level(floatVal)
    updateVolume(_ => volume')
    let sound = blipSoundRef.current
    Howler.volume(sound, floatVal)
    if !Howler.playing(sound) {
      Howler.play(sound)
    }
  }

  let volumeIcon = switch volume {
  | Mute(_) => "emoji_speaker_muted"
  | Level(_) => "emoji_speaker"
  }

  let handleVolumeIconClick = _event => {
    let volume' = switch volume {
    | Mute(n) if n == 0.0 => ClientSettings.defaults.volume
    | Mute(n) => ClientSettings.Level(n)
    | Level(n) => Mute(n)
    }
    updateVolume(_ => volume')
  }

  let onClientProfileTypeChanged = event => {
    let client_profile_type = switch (event->ReactEvent.Form.target)["value"] {
    | "feminine" => ClientSettings.Feminine
    | "machine" => ClientSettings.Machine
    | "abstract" => ClientSettings.Abstract
    | _ => Masculine
    }

    updateClientProfileType(_ => client_profile_type)
  }

  let _onChangeInitials = event => {
    let inputVal = (event->ReactEvent.Form.target)["value"]
    let inputStripped =
      Js.String.split("", inputVal)
      ->Belt.Array.map(Js.String.toUpperCase)
      ->Belt.Array.keepMap(Js.String.match_(%re("/[A-Za-z]/")))
      ->(Js.Array.joinWith("", _))
    updateClientInitials(_ => inputStripped)
  }

  let onChangeClientId = _event => updateClientId(_ => Nanoid.nanoid())

  let onSaveClick = _event => {
    let settings = LocalStorage.getClientSettings()
    onSave({
      open ClientSettings
      {
        ...settings,
        volume,
        client_id,
        client_profile_type,
        client_initials,
      }
    })
    RescriptReactRouter.replace("./")
  }

  <div
    className="bg-white shadow-md border border-solid border-gray-300 rounded px-8 pt-6 pb-8 mb-4 w-10/12">
    <div className="mb-4 text-xl text-center"> {React.string("Settings")} </div>
    <div className="mb-4 text-lg"> {React.string("Profile")} </div>
    <div className="flex flex-col">
      <img
        src={LibAvatar.getAvatarUri(~client_id, ~client_profile_type)}
        className="self-center rounded border border-gray-300 p-2 w-1/3 cursor-pointer"
        onClick=onChangeClientId
      />
      <div className="flex flex-col flex-grow mt-4">
        <div className="flex justify-between">
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-male"
              className="ml-auto mr-auto"
              name="client-profile-type"
              value="masculine"
              defaultChecked={client_profile_type == Masculine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-male"> {React.string("Man")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-female"
              className="ml-auto mr-auto"
              name="client-profile-type"
              value="feminine"
              defaultChecked={client_profile_type == Feminine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-female"> {React.string("Woman")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-machine"
              className="ml-auto mr-auto"
              name="client-profile-type"
              value="machine"
              defaultChecked={client_profile_type == Machine}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-machine"> {React.string("Bot")} </label>
          </div>
          <div className="flex flex-col">
            <input
              type_="radio"
              id="client-profile-abstract"
              className="ml-auto mr-auto"
              name="client-profile-type"
              value="abstract"
              defaultChecked={client_profile_type == Abstract}
              onChange=onClientProfileTypeChanged
            />
            <label htmlFor="client-profile-abstract"> {React.string("Abstract")} </label>
          </div>
        </div>
        // <div className="mt-4">
        //   <label htmlFor="client-initials"> {React.string("Initials: ")} </label>
        //   <input
        //     style={ReactDOM.Style.make(~maxWidth="10em", ())}
        //     placeholder="Enter your initials"
        //     id="client-initials"
        //     onChange=onChangeInitials
        //     className="focus:outline-0 border border-transparent focus:bg-white focus:border-gray-300
        //                placeholder-gray-600 rounded-lg bg-gray-200 p-2 ml-4 appearance-none leading-normal
        //                text-center"
        //     type_="text"
        //     maxLength=2
        //     pattern="[A-Za-z]*"
        //     value=client_initials
        //   />
        // </div>
      </div>
    </div>
    <form className="mt-8">
      <div className="mb-4 text-lg"> {React.string("Sound Effects Volume")} </div>
      <div className="mb-4 flex flex-row h-8">
        <div
          className="bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold rounded inline-flex items-center cursor-pointer"
          onClick=handleVolumeIconClick>
          <img
            src={`./static/img/${volumeIcon}.svg`}
            className="block mx-4"
            style={ReactDOM.Style.make(
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
          min="0"
          max="1"
          value={Js.Float.toString(volumeLevel)}
          step=0.05
          onChange=handleVolumeChange
          className="flex-grow ml-4 cursor-pointer"
        />
      </div>
      <div className="flex items-center justify-around">
        <div onClick={_ => RescriptReactRouter.replace("./")} className="link link-blue" href="#">
          {React.string("Cancel")}
        </div>
        <button onClick=onSaveClick className="btn btn-blue" type_="button">
          {React.string("Save")}
        </button>
      </div>
    </form>
  </div>
}
