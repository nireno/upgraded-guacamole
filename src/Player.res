@decco
type hand = list<Card.t>

@decco
type id = Quad.id

@ocaml.doc(" 
  As opposed to the regular id_encode provided by [@decco]
  I use this for my custom (recursive) game phase encoding
")
let getPartner: Quad.id => Quad.id = x =>
  switch x {
  | N1 => N3
  | N2 => N4
  | N3 => N1
  | N4 => N2
  }

let playersAsQuad = (~startFrom=Quad.N1, ()) => {
  let a = startFrom
  let b = Quad.nextId(a)
  let c = Quad.nextId(b)
  let d = Quad.nextId(c)
  (a, b, c, d)
}

@ocaml.doc(" How many turns does it take to get from playerA to playerB 
   Returns 0, 1, 2 or 3
   Example: turnDistance(P2, P1) = 3 
")
let turnDistance = (playerA, playerB) => {
  let rec f = (n, a, b) => a == b ? n : f(n + 1, Quad.nextId(a), b)
  f(0, playerA, playerB)
}

let stringOfId = x =>
  switch x {
  | Quad.N1 => "Player 1"
  | N2 => "Player 2"
  | N3 => "Player 3"
  | N4 => "Player 4"
  }

let stringOfMaybeId = x =>
  switch x {
  | None => "None"
  | Some(player) => stringOfId(player)
  }

@decco
type begPhaseContext = PlayerBegPhaseDeciding | PlayerBegPhaseStanding

@decco
type phase =
  | PlayerIdlePhase
  | PlayerTurnPhase(id)
  | PlayerDealPhase
  | PlayerBegPhase(begPhaseContext)
  | PlayerGiveOnePhase
  | PlayerRunPackPhase
  | PlayerFlipFinalTrumpPhase
  | PlayerRedealPhase

let stringOfPhase = x =>
  switch x {
  | PlayerIdlePhase => "PlayerIdlePhase"
  | PlayerTurnPhase(id) =>
    let str_playerId = stringOfId(id)
    j`PlayerTurnPhase($str_playerId)`
  | PlayerDealPhase => "PlayerDealPhase"
  | PlayerBegPhase(PlayerBegPhaseDeciding) => "PlayerBegPhase(PlayerBegPhaseDeciding)"
  | PlayerBegPhase(PlayerBegPhaseStanding) => "PlayerBegPhase(PlayerBegPhaseStanding)"
  | PlayerGiveOnePhase => "PlayerGiveOnePhase"
  | PlayerRunPackPhase => "PlayerRunPackPhase"
  | PlayerFlipFinalTrumpPhase => "PlayerFlipFinalTrumpPhase"
  | PlayerRedealPhase => "PlayerRedealPhase"
  }

let maybeIdEqual = (maybeId, id) => Js.Option.isSomeValue((. x, y) => x == y, id, maybeId)

@react.component
let make = (
  ~sendDeal,
  ~sendBeg,
  ~sendStandUp,
  ~sendGiveOne,
  ~sendRunPack,
  ~sendReshuffle,
  ~sendKickFinalTrump,
  ~playerPhase=PlayerIdlePhase,
) =>
  switch playerPhase {
  | PlayerDealPhase =>
    <Modal visible=true>
      <button className="btn btn-blue" onClick=sendDeal> {React.string("Deal")} </button>
    </Modal>
  | PlayerBegPhase(PlayerBegPhaseDeciding) =>
    <Modal visible=true>
      <button className="btn btn-blue m-2" onClick=sendBeg> {React.string("Beg")} </button>
      <button className="btn btn-blue m-2" onClick=sendStandUp> {React.string("Stand")} </button>
    </Modal>
  | PlayerBegPhase(PlayerBegPhaseStanding) => React.null
  | PlayerGiveOnePhase =>
    <Modal visible=true>
      <button className="btn btn-blue m-2" onClick=sendGiveOne> {React.string("Give One")} </button>
      <button className="btn btn-blue m-2" onClick=sendRunPack> {React.string("Run Pack")} </button>
    </Modal>
  | PlayerRunPackPhase =>
    <Modal visible=true>
      <button className="btn btn-blue" onClick=sendRunPack> {React.string("Run Again")} </button>
    </Modal>
  | PlayerFlipFinalTrumpPhase =>
    <Modal visible=true>
      <button className="btn btn-blue" onClick=sendKickFinalTrump>
        {React.string("Flip Again")}
      </button>
    </Modal>
  | PlayerRedealPhase =>
    <Modal visible=true>
      <button className="btn btn-blue" onClick=sendReshuffle> {React.string("Reshuffle")} </button>
    </Modal>
  | PlayerTurnPhase(_)
  | PlayerIdlePhase => React.null
  }
