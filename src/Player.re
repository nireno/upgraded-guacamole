[@decco]
type hand = list(Card.t);

[@decco]
type id =
  | P1
  | P2
  | P3
  | P4;

/** 
  As opposed to the regular id_encode provided by [@decco]
  I use this for my custom (recursive) game phase encoding
*/
let stringifyId = fun
| P1 => "P1"
| P2 => "P2"
| P3 => "P3"
| P4 => "P4";

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

let playersAsQuad = (~startFrom=P1, ()) => {
  let a = startFrom;
  let b = nextPlayer(a);
  let c = nextPlayer(b);
  let d = nextPlayer(c);
  (a, b, c, d)
};


/** How many turns does it take to get from playerA to playerB 
   Returns 0, 1, 2 or 3
   Example: turnDistance(P2, P1) = 3 
*/
let turnDistance = (playerA, playerB) => {
   let rec f = (n, a, b) => {
     a == b ? n : f(n+1, nextPlayer(a), b);
   }
   f(0, playerA, playerB)
};

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
  | PlayerRunPackPhase
  | PlayerRedealPhase;

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
  | PlayerRunPackPhase => "PlayerRunPackPhase"
  | PlayerRedealPhase => "PlayerRedealPhase";


let maybeIdEqual = (maybeId, id) =>
  Js.Option.isSomeValue((. x, y) => x == y, id, maybeId);

[@react.component]
let make =
    (
      ~sendDeal,
      ~sendBeg,
      ~sendStandUp,
      ~sendGiveOne,
      ~sendRunPack,
      ~sendReshuffle,
      ~playerPhase=PlayerIdlePhase,
    ) => {
      {switch (playerPhase) {
        | PlayerDealPhase =>
          <Modal visible=true>
            <button className="btn btn-blue" onClick=sendDeal> {ReasonReact.string("Deal")} </button>
          </Modal>
        | PlayerBegPhase =>
          <Modal visible=true>
            <button className="btn btn-blue m-2" onClick=sendBeg> {ReasonReact.string("Beg")} </button>
            <button className="btn btn-blue m-2" onClick=sendStandUp>
              {ReasonReact.string("Stand")}
            </button>
          </Modal>
        | PlayerGiveOnePhase =>
          <Modal visible=true>
            <button className="btn btn-blue m-2" onClick=sendGiveOne>
              {ReasonReact.string("Give One")}
            </button>
            <button className="btn btn-blue m-2" onClick=sendRunPack>
              {ReasonReact.string("Run Pack")}
            </button>
          </Modal>
        | PlayerRunPackPhase =>
          <Modal visible=true>
            <button className="btn btn-blue" onClick=sendRunPack>
              {ReasonReact.string("Run Again")}
            </button>
          </Modal>
        | PlayerRedealPhase =>
          <Modal visible=true>
            <button className="btn btn-blue" onClick=sendReshuffle>
              {ReasonReact.string("Reshuffle")}
            </button>
          </Modal>
        | PlayerTurnPhase(_)
        | PlayerIdlePhase => ReasonReact.null
        }}
};
