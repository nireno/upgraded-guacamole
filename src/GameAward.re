[@decco] 
type jackAward = 
  | HangJackAward
  | RunJackAward;

let jackAwardValue = fun
  | HangJackAward => 3
  | RunJackAward => 1;

let stringOfJackAward = fun
  | HangJackAward => "Hang Jack"
  | RunJackAward => "Run Jack";


[@decco]
type award =
  | HighAward
  | LowAward
  | JackAward(jackAward)
  | GameAward;

// A decisive point can end the game before the round is over
[@decco]
type decisiveAward =
  | KickDecides(Card.t)
  | HighDecides(Card.t)
  | LowDecides(Card.t)
  | RunJackDecides
  | HangJackDecides
  | HighAndLowDecides(Card.t, Card.t)
  | HighAndRunJackDecides(Card.t)
  | HighAndHangJackDecides(Card.t)
  | LowAndRunJackDecides(Card.t)
  | LowAndHangJackDecides(Card.t)
  | HighLowAndRunJackDecides(Card.t, Card.t)
  | HighLowAndHangJackDecides(Card.t, Card.t);

let value =
  fun
  | HighAward
  | LowAward
  | GameAward => 1
  | JackAward(jackAward) => jackAwardValue(jackAward);

let toString = fun 
| HighAward => "High"
| LowAward => "Low"
| GameAward => "Game"
| JackAward(jackAward) => stringOfJackAward(jackAward);
