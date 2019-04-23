open AppPrelude;
type award =
  | HighAward
  | LowAward
  | RunJackAward
  | HangJackAward
  | GameAward;

let valueOfAward =
  fun
  | HighAward
  | LowAward
  | GameAward
  | RunJackAward => 1
  | HangJackAward => 3;

let kickPoints =
  Card.Rank.(
    fun
    | Ace => 1
    | Six => 2
    | Jack => 3
    | _ => 0
  );

type phase =
  | FindPlayersPhase(int)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase
  | RoundSummaryPhase
  | PackDepletedPhase
  | GameOverPhase;

let debugPhase = (phase, ~depth=0, ()) => {
  debuggin("phase: ", ~depth, ());
  let depth = depth + 1;
  switch (phase) {
  | FindPlayersPhase(n) => debuggin({j|FindPlayersPhase($n)|j}, ~depth, ())
  | DealPhase => debuggin("DealPhase", ~depth, ())
  | BegPhase => debuggin("BegPhase", ~depth, ())
  | GiveOnePhase => debuggin("GiveOnePhase", ~depth, ())
  | RunPackPhase => debuggin("RunPackPhase", ~depth, ())
  | PlayerTurnPhase => debuggin("PlayerTurnPhase", ~depth, ())
  | RoundSummaryPhase => debuggin("RoundSummaryPhase", ~depth, ())
  | PackDepletedPhase => debuggin("PackDepletedPhase", ~depth, ())
  | GameOverPhase => debuggin("GameOverPhase", ~depth, ())
  };
};
