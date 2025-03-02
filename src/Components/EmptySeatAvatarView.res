@react.component
let make = () => {
  let opacityMax = "0.5"
  let opacityMin = "0.1"
  let intervalMilliseconds = 1500

  let (isOpaque, updateOpacity) = React.useState(() => true)

  My.React.useInterval(
    () => updateOpacity(isOpaquePrev => !isOpaquePrev),
    Some(intervalMilliseconds),
  )

  <div className="border border-gray-300 rounded">
    <div
      className="w-full"
      style={ReactDOM.Style.make(
        ~opacity=isOpaque ? opacityMax : opacityMin,
        ~transition="opacity 1s ease-out",
        (),
      )}>
      <Svg_Decor_AvatarPlaceholder />
    </div>
  </div>
}
