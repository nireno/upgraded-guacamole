[@bs.val] external node_env: string = "process.env.NODE_ENV";
open ClientGame;

let isPlayerTurn = (turn, playerId) => {
  switch (turn) {
  | None => false
  | Some(turn) => turn == playerId
  };
};

let handToPlayerCards: (Player.id, Hand.t) => list((Player.id, Card.t)) =
  (player, hand) => {
    List.map(card => (player, card), hand);
  };

let playerPhase: (Game.phase, Player.id, Player.id, option(Player.id), Player.id) => Player.phase =
  (gamePhase, dealer, leader, maybePlayerTurn, player) => {
    Player.maybeIdEqual(maybePlayerTurn, player)
      ? Player.PlayerTurnPhase
      : dealer == player && gamePhase == DealPhase
          ? PlayerDealPhase
          : dealer == player && gamePhase == GiveOnePhase
              ? PlayerGiveOnePhase
              : dealer == player && gamePhase == RunPackPhase
                  ? PlayerRunPackPhase
                  : leader == player && gamePhase == BegPhase ? PlayerBegPhase : PlayerIdlePhase;
  };

module SockClient = BsSocket.Client.Make(SocketMessages);
let socket = SockClient.create();
SockClient.emit(socket, Ping);

module App = {
  let component = ReasonReact.reducerComponent("AllFoursApp");

  let make = _children => {
    ...component,
    initialState: ClientGame.initialState,
    reducer: ClientGame.reducer,
    didMount: ({send}) => {
      SockClient.on(socket, x =>
        switch (x) {
        | Start => Js.log("Got Start")
        | SetState(jsonString) =>
          let state = stateOfJson(jsonString);
          debugState(state, ());
          send(SetState(state));
        }
      );
    },

    render: self => {
      let {ReasonReact.state, send: _send} = self;
      // let sendActionEvent = (action, _event) => send(action);

      let _createPlayerTricks = tricks =>
        <div className="column">
          {List.length(tricks) == 0
             ? <div> {ReasonReact.string("No tricks")} </div>
             : <div>
                 {List.map(
                    trick =>
                      <div key={Trick.stringOfTrick(trick)} className="section">
                        <Trick trick />
                      </div>,
                    tricks,
                  )
                  |> Belt.List.toArray
                  |> ReasonReact.array}
               </div>}
        </div>;
      <div>
        <div> {ReasonReact.string(Player.stringOfId(state.me))} </div>
        {
          switch (state.phase) {
          | FindPlayersPhase(n) =>
            let playersAsText = n == 1 ? "player" : "players";
            let nAsText = string_of_int(n);
            <div> {ReasonReact.string({j|Finding $nAsText more $playersAsText ...|j})} </div>;
          | DealPhase => ReasonReact.null
          | _ => ReasonReact.null
          };
        }
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<App />, "app");
