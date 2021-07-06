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
    <img
      src="./static/img/avatar_placeholder.svg"
      className="w-full "
      style={ReactDOMRe.Style.make(
        ~opacity=isOpaque ? opacityMax : opacityMin,
        ~transition="opacity 1s ease-out",
        (),
      )}
    />
  </div>
}
