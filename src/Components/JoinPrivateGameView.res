type formState =
  | Initial
  | Query
  | Error

@react.component
let make = (~sendJoinGame, ~inviteCode as maybeInviteCode=?) => {
  let (inviteCode, updateInviteCode) = React.useState(() =>
    maybeInviteCode->Belt.Option.getWithDefault("")
  )
  let (state, updateState) = React.useState(() => Initial)
  let (canJoin, setCanJoin) = React.useState(() => true)

  let doJoin = inviteCode => {
    setCanJoin(_ => false)
    let ackJoinGame = response => {
      setCanJoin(_ => true)
      switch response {
      | SocketMessages.AckOk =>
        updateState(_ => Initial)
        RescriptReactRouter.replace("./")
      | _ => updateState(_ => Error)
      }
    }
    updateState(_ => Query)
    sendJoinGame(inviteCode, ackJoinGame)
  }

  React.useEffect0(() => {
    switch maybeInviteCode {
    | None => ()
    | Some(inviteCode) => doJoin(inviteCode)
    }
    None
  })

  let onJoinClick = _event => doJoin(inviteCode)

  let handleInputChanged = event => {
    let value = (event->ReactEvent.Form.target)["value"]
    updateInviteCode(_ => value)
  }

  <div
    className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4 w-11/12">
    {switch state {
    | Initial => <>
        <div className="mb-4 text-xl text-center">
          {React.string("Enter your invite code")}
        </div>
        <div>
          <form className="flex flex-row justify-center mb-4">
            <input
              className="bg-white focus:outline-0 focus:shadow-outline border border-gray-300 rounded-lg py-2 px-4 block appearance-none leading-normal"
              type_="text"
              autoFocus=true
              minLength=8
              maxLength=15
              placeholder="Enter your invite code"
              defaultValue=inviteCode
              onChange=handleInputChanged
            />
          </form>
        </div>
        <div className="flex items-center justify-around">
          <div onClick={_ => RescriptReactRouter.replace("../")} className="link link-blue" href="#">
            {React.string("Cancel")}
          </div>
          <button
            onClick=onJoinClick
            disabled={!canJoin}
            className={"btn btn-blue " ++ (canJoin ? "" : "btn-disabled")}
            type_="button">
            {React.string("Join Game")}
          </button>
        </div>
      </>
    | Query => <div className="text-xl text-center"> {React.string("Searching...")} </div>
    | Error =>
      <div className="text-center">
        <div className="text-xl"> {React.string("No games found")} </div>
        <div> {React.string("Verify your invite link and try again.")} </div>
        <button
          onClick={_ => RescriptReactRouter.replace("./")}
          className="btn btn-blue mt-4"
          type_="button">
          {React.string("Ok")}
        </button>
      </div>
    }}
  </div>
}
