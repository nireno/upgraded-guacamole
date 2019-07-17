type formState =
  | Initial
  | Query
  | Error;

[@react.component]
let make = (~sendJoinGame) => {
  let (inviteCode, updateInviteCode) = React.useState(() => "");
  let (state, updateState) = React.useState(() => Initial);

  let submit = _event => {
    let ackJoinGame =
      fun
      | SocketMessages.AckOk => {
          updateState(_ => Initial);
          ReasonReactRouter.replace("/");
        }
      | _ => updateState(_ => Error);
    updateState(_ => Query);
    sendJoinGame(inviteCode, ackJoinGame);
  };

  let handleInputChanged = event => {
    let value = event->ReactEvent.Form.target##value;
    updateInviteCode(_ => value);
  };

  <div
    className="bg-white shadow-md border border-solid border-gray-200 rounded px-8 pt-6 pb-8 mb-4 w-11/12">
    {switch (state) {
     | Initial =>
       <>
         <div className="mb-4 text-xl text-center">
           {ReasonReact.string("Enter your invite code")}
         </div>
         <div>
           <form className="flex flex-row justify-center mb-4">
             <input
               className="bg-white focus:outline-0 focus:shadow-outline border border-gray-300 rounded-lg py-2 px-4 block appearance-none leading-normal"
               type_="text"
               autoFocus=true
               minLength=8
               maxLength=12
               placeholder="Enter your invite code"
               defaultValue=inviteCode
               onChange=handleInputChanged
             />
           </form>
         </div>
         <div className="flex items-center justify-around">
           <div onClick={_ => ReasonReactRouter.push("../")} className="link link-blue" href="#">
             {ReasonReact.string("Cancel")}
           </div>
           <button onClick=submit className="btn btn-blue" type_="button">
             {ReasonReact.string("Join Game")}
           </button>
         </div>
       </>
     | Query => <div className="text-xl text-center"> {ReasonReact.string("Searching...")} </div>
     | Error =>
       <div className="text-center">
         <div className="text-xl"> {ReasonReact.string("No games found")} </div>
         <div> {ReasonReact.string("Verify your invite code and try again.")} </div>
         <button
           onClick={_ => updateState(_ => Initial)} className="btn btn-blue mt-4" type_="button">
           {ReasonReact.string("Ok")}
         </button>
       </div>
     }}
  </div>;
};
