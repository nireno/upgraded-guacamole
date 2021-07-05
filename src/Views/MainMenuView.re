type socketMessages;
type gameStats = {
  playersOnline: int,
  activeGames: int,
};
type status =
  | Loading
  | Loaded(gameStats);

// module LiveMessages = {
//   type serverToClient =
//     | Unknown
//     | ServerPublicState(int /* playersOnline */, int /* active games */);
//   type clientToServer;
// };

// module MainMenuSocketClient = BsSocketio.Client.Make(LiveMessages);

// external castClient: ClientSocket.T.t => MainMenuSocketClient.t = "%identity";

[@react.component]
let make = (~socket: ClientSocket.T.t) => {
  let (status, setStatus) = React.useState(() => Loading);
  // let socket = castClient(socket);

  React.useEffect0(() => {
    ClientSocket.T.on(socket, msg => {
      switch (msg) {
      | ServerPublicState(playersOnline, activeGames) =>
        setStatus(_ => Loaded({playersOnline, activeGames}))
      | _ => ()
      }
    });
    None;
  });

  switch (status) {
  | Loading => <MenuView> <div> "Loading..."->React.string </div> </MenuView>
  | Loaded({playersOnline, activeGames}) =>
    <MenuView>
      <div>
        "Players Online"->React.string
        <span> {playersOnline->string_of_int->React.string} </span>
      </div>
      <div>
        "Active Games"->React.string
        <span> {activeGames->string_of_int->React.string} </span>
      </div>
    </MenuView>
  };
};
