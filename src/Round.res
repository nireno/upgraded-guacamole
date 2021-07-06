@ocaml.doc(" One round is completed when all players have played *all* their cards ")
type tally = {
  low: Team.id,
  high: Team.id,
  hangJack: option<Team.id>,
}
// let tally = (round, trumpSuit) => {
//   switch(isComplete(round)){
//     | false => Belt.Result.Error("round-is-incomplete: " ++ stringOfRound(round))
//     | true =>
//   }
// };
