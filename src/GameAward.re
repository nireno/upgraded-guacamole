[@decco]
type luckyAwardData = {
  team_id: Team.id,
  winning_card: Card.t,
  losing_card_maybe: option(Card.t),
};

[@decco]
type gameAwardData = {
  team_id_maybe: option(Team.id),
  winning_count: int,
  losing_count: int,
};

[@decco]
type jackAwardType =
  | HangJackAward
  | RunJackAward;

[@decco]
type jackAwardData = {
  team_id: Team.id,
  jack_award_type: jackAwardType,
};

let jackAwardValue =
  fun
  | HangJackAward => Ruleset.default.hangJackAwardValue
  | RunJackAward => Ruleset.default.runJackAwardValue;

let stringOfJackAward =
  fun
  | HangJackAward => "Hang Jack"
  | RunJackAward => "Run Jack";

let stringOfLuckyAwardData = (d: luckyAwardData) => {
  let team = d.team_id->Team.stringOfTeam;
  let card1 = d.winning_card->Card.stringOfCard;
  let card2 = d.losing_card_maybe->Card.stringOfMaybeCard;
  {j|{$team, $card1, $card2}|j};
};

let stringOfJackAwardData = (d: jackAwardData) => {
  let teamText = d.team_id->Team.stringOfTeam;
  let jackText = d.jack_award_type->stringOfJackAward;
  {j|{$teamText, $jackText}|j};
};
