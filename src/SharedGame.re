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
  | FindSubsPhase(int, phase)
  | FindPlayersPhase(int)
  | DealPhase
  | BegPhase
  | GiveOnePhase
  | RunPackPhase
  | PlayerTurnPhase
  | RoundSummaryPhase
  | PackDepletedPhase
  | GameOverPhase;

let rec stringOfPhase = fun
  | FindSubsPhase(n, phase) => "FindSubsPhase(" ++ string_of_int(n) ++ ", " ++ stringOfPhase(phase) ++ ")"
  | FindPlayersPhase(n) => "FindPlayersPhase(" ++ string_of_int(n) ++ ")"
  | DealPhase => "DealPhase"
  | BegPhase => "BegPhase"
  | GiveOnePhase => "GiveOnePhase"
  | RunPackPhase => "RunPackPhase"
  | PlayerTurnPhase => "PlayerTurnPhase"
  | RoundSummaryPhase => "RoundSummaryPhase"
  | PackDepletedPhase => "PackDepletedPhase"
  | GameOverPhase => "GameOverPhase";


let debugPhase = (phase, ~depth=0, ()) => {
  debuggin("phase: ", ~depth, ());
  let depth = depth + 1;
  debuggin(stringOfPhase(phase), ~depth, ())
};
