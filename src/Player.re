[@decco]
type hand = list(Card.t);

[@decco]
type id =
  | P1
  | P2
  | P3
  | P4;

let nextPlayer =
  fun
  | P1 => P2
  | P2 => P3
  | P3 => P4
  | P4 => P1;

let prevPlayer =
  fun
  | P1 => P4
  | P2 => P1
  | P3 => P2
  | P4 => P1;

let stringOfId =
  fun
  | P1 => "Player 1"
  | P2 => "Player 2"
  | P3 => "Player 3"
  | P4 => "Player 4";

let stringOfMaybeId = fun
  | None => "None"
  | Some(player) => stringOfId(player);

[@decco]
type phase =
  | PlayerIdlePhase
  | PlayerTurnPhase(id)
  | PlayerDealPhase
  | PlayerBegPhase
  | PlayerGiveOnePhase
  | PlayerRunPackPhase;

let stringOfPhase =
  fun
  | PlayerIdlePhase => "PlayerIdlePhase"
  | PlayerTurnPhase(id) => {
      let str_playerId = stringOfId(id);
      {j|PlayerTurnPhase($str_playerId)|j};
    }
  | PlayerDealPhase => "PlayerDealPhase"
  | PlayerBegPhase => "PlayerBegPhase"
  | PlayerGiveOnePhase => "PlayerGiveOnePhase"
  | PlayerRunPackPhase => "PlayerRunPackPhase";


let maybeIdEqual = (maybeId, id) =>
  Js.Option.isSomeValue((. x, y) => x == y, id, maybeId);

[@react.component]
let make =
    (
      ~id,
      ~sendDeal,
      ~sendBeg,
      ~sendStandUp,
      ~sendGiveOne,
      ~sendRunPack,
      ~playerPhase=PlayerIdlePhase,
    ) => {
    <div className="player__actions flex justify-around my-4">
      {switch (playerPhase) {
        | PlayerDealPhase =>
          <button className="btn btn-blue" onClick=sendDeal> {ReasonReact.string("Deal")} </button>
        | PlayerBegPhase =>
          <>
            <button className="btn btn-blue m-2" onClick=sendBeg> {ReasonReact.string("Beg")} </button>
            <button className="btn btn-blue m-2" onClick=sendStandUp>
              {ReasonReact.string("Stand")}
            </button>
          </>
        | PlayerGiveOnePhase =>
          <>
            <button className="btn btn-blue m-2" onClick=sendGiveOne>
              {ReasonReact.string("Give One")}
            </button>
            <button className="btn btn-blue m-2" onClick=sendRunPack>
              {ReasonReact.string("Run Pack")}
            </button>
          </>
        | PlayerRunPackPhase =>
          <button className="btn btn-blue" onClick=sendRunPack>
            {ReasonReact.string("Run Again")}
          </button>
        | PlayerTurnPhase(_)
        | PlayerIdlePhase => ReasonReact.null
        }}
    </div>
};
