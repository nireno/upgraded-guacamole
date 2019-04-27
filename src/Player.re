type hand = list(Card.t);

type id =
  | P1
  | P2
  | P3
  | P4;

// type state = {
//   hand,
//   tricks: list(Trick.t),
//   maybeTrumpCard: option(Card.t),
//   maybeLeadCard: option(Card.t),
//   dealer: id,
//   leader: id,
//   maybePlayerTurn: option(id),
//   team1Points: int,
//   team2Points: int,
//   maybeTeamHigh: option(Team.id),
//   maybeTeamLow: option(Team.id),
//   maybeTeamJack: option((Team.id, Game.award)),
//   maybeTeamGame: option(Team.id),
// };

// let initializeState = () => {
//   hand: [],
//   tricks: [],
//   maybeTrumpCard: None,
//   maybeLeadCard: None,
//   dealer: P1,
//   leader: P2,
//   maybePlayerTurn: None,
//   team1Points: 0,
//   team2Points: 0,
//   maybeTeamHigh: None,
//   maybeTeamLow: None,
//   maybeTeamJack: None,
//   maybeTeamGame: None,
// };

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
         | PlayerTurnPhase(_)
         | PlayerIdlePhase => ReasonReact.null
         }}
      </div>
    </div>,
};
