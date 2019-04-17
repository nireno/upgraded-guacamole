type hand = list(Card.t);
// type lift = list((Card.t, Card.t, Card.t, Card.t));

type id =
  | P1
  | P2
  | P3
  | P4;

let firstPlayer = P1;
let lastPlayer = P4;

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

type phase =
  | PlayerIdlePhase
  | PlayerTurnPhase
  | PlayerDealPhase
  | PlayerBegPhase
  | PlayerGiveOnePhase
  | PlayerRunPackPhase;

let maybeIdEqual = (maybeId, id) =>
  Js.Option.isSomeValue((. x, y) => x == y, id, maybeId);

let component = ReasonReact.statelessComponent("Hand");

let make =
    (
      ~id,
      ~sendDeal,
      ~sendBeg,
      ~sendStandUp,
      ~sendGiveOne,
      ~sendRunPack,
      ~playerPhase=PlayerIdlePhase,
      _children,
    ) => {
  ...component,
  render: _self =>
    <div className="column player">
      <div> {ReasonReact.string(stringOfId(id))} </div>
      <div className="player__actions">
        {switch (playerPhase) {
         | PlayerDealPhase =>
           <button onClick=sendDeal> {ReasonReact.string("Deal")} </button>
         | PlayerBegPhase =>
           <>
             <button onClick=sendBeg> {ReasonReact.string("Beg")} </button>
             <button onClick=sendStandUp>
               {ReasonReact.string("Stand")}
             </button>
           </>
         | PlayerGiveOnePhase =>
           <>
             <button onClick=sendGiveOne>
               {ReasonReact.string("Give One")}
             </button>
             <button onClick=sendRunPack>
               {ReasonReact.string("Run Pack")}
             </button>
           </>
         | PlayerRunPackPhase =>
           <button onClick=sendRunPack>
             {ReasonReact.string("Run Again")}
           </button>
         | PlayerTurnPhase
         | PlayerIdlePhase => ReasonReact.null
         }}
      </div>
    </div>,
};
