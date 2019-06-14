[@decco]
type award =
  | HighAward
  | LowAward
  | RunJackAward
  | HangJackAward
  | GameAward;

let value =
  fun
  | HighAward
  | LowAward
  | GameAward
  | RunJackAward => 1
  | HangJackAward => 3;
