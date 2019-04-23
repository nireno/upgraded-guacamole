open AppPrelude;
type clientToServer =
  | Ping
  | Deal;

type serverToClient =
  | SetState(strJson)
  | Start;

let debugServerMsg =
  fun
  | SetState(json) => {
      let state = ClientGame.stateOfJson(json);
      debuggin("SetState", ());
      ClientGame.(debugState(state));
    }
  | Start => debuggin("Start");
