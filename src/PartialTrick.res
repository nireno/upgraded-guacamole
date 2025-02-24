@@ocaml.doc(" One trick is completed when each player play's one card on the board ")

type partialTrick = {
  p1CardSlot: option<Card.t>,
  p2CardSlot: option<Card.t>,
  p3CardSlot: option<Card.t>,
  p4CardSlot: option<Card.t>,
}

let listOfTrick: partialTrick => list<(Player.id, option<Card.t>)> = r => list{
  (Quad.N1, r.p1CardSlot),
  (N2, r.p2CardSlot),
  (N3, r.p3CardSlot),
  (N4, r.p4CardSlot),
}

let stringOfTrick = r => {
  let stringOfPlayerCardSlot = ((playerId, cardSlot)) =>
    Player.stringOfId(playerId) ++ (": " ++ Card.stringOfMaybeCard(cardSlot))

  List.fold_left(
    (acc, s) => acc ++ (" " ++ s),
    "",
    List.map(stringOfPlayerCardSlot, listOfTrick(r)),
  )
}

let isComplete = partialTrick =>
  Belt.List.every(
    List.map(((_playerId, cardSlot: option<Card.t>)) => cardSlot, listOfTrick(partialTrick)),
    Belt.Option.isSome,
  )
