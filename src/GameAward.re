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

let toString = fun 
| HighAward => "High"
| LowAward => "Low"
| GameAward => "Game"
| RunJackAward => "Run Jack"
| HangJackAward => "Hang Jack";
