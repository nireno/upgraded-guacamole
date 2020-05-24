module Foo = {
    type ioGameStates = str_json;

  type clientToServer = None;
  type serverToClient = 
    | IO_UpdateGameStates(ioGameStates);
}

module PlayerState = {
  [@react.component]
  let make = (~playerId, ~playerState) => {
    <div className="player_state">
      <div> {playerId |> Player.stringOfId |> ReasonReact.string} </div>
      <div>
      {
        switch(playerState.ClientGame.pla_profile_maybe){
          | None => "not connected"
          | Some(profile) => profile.client_username ++ " connected"
        }
        |> ReasonReact.string
      }
      </div>
    </div>;
  };
};

module GameState = {
  [@react.component]
  let make = (~gameState) => {
    <pre className="game-state">
      {ReasonReact.string(
         Game.debugOfState(gameState) |> Obj.magic |> Js.Json.stringifyWithSpace(_, 2),
       )}
    </pre>;
  };
};

[@react.component]
let make = (~gameStates) => {

  let (gamesStates, updateGameStates) = React.useState(() => gameStates);
  React.useEffect0(
      () => {
        let socket = ClientSocket.Admin.create();
        // setMaybeSocket(_ => Some(socket));
        ClientSocket.Admin.on(socket, msg => 
          switch (msg) {
          | IO_UpdateGameStates(ioGameStates) => 
            switch (Join.gameStates_decode(ioGameStates |> Js.Json.parseExn)) {
            | Belt.Result.Error(err) => Js.log(err)
            | Belt.Result.Ok(state) =>
              updateGameStates(_ => state);
            }
          });
        //   switch (x) {
        //   | SetState(ioClientState) =>
        //     switch (ClientGame.state_decode(ioClientState |> Js.Json.parseExn)) {
        //     | Belt.Result.Error(err) => Js.log(err)
        //     | Belt.Result.Ok(state) =>
        //       dispatch(MatchServerState(state));
        //     }
        //   | ShowToast(ioToast) =>
        //     switch (Noti.t_decode(ioToast |> Js.Json.parseExn)) {
        //     | Belt.Result.Error(err) => Js.log(err)
        //     | Belt.Result.Ok(toast) => updateNotis(AddOne(toast))
        //     }
        //   | Reset =>
        //     dispatch(MatchServerState(ClientGame.initialState));
        //     updateNotis(Reset(Noti.State.initial));
        //   | AckOk | AckError(_) => ()
        //   }
        // );
        None;
      }
    );

  <div className="dash"> 
  {
    gameStates
    ->Belt.Array.map(gameState =>
        <GameState key={Game.stringOfGameId(gameState.Game.game_id)} gameState />
      )
    ->ReasonReact.array;
  }
  </div>;
};
