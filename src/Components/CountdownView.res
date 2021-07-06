@react.component
let make = (~from, ~className=?, ~style=?) => {
  let (countdown, setCountdown) = React.useState(() => from)
  My.React.useInterval(
    () => setCountdown(prevCountdown => prevCountdown <= 0 ? 0 : prevCountdown - 1),
    countdown <= 0 ? None : Some(1000),
  )
  <div ?className ?style> {React.string(countdown |> string_of_int)} </div>
}
