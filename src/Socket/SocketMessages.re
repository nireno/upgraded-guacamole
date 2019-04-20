type clientToServer =
  | Deal
  | NewRound;

type serverToClient =
  | Ok
  | Start
  | Fail;
