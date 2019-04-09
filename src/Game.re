type award =
  | High
  | Low
  | RunJack
  | HangJack
  | Game;

let valueOfAward =
  fun
  | High
  | Low
  | Game
  | RunJack => 1
  | HangJack => 3;

let kickPoints =
  Card.Rank.(
    fun
    | Ace => 1
    | Six => 2
    | Jack => 3
    | _ => 0
  );

type phase =
  | RoundSummary
  | Game
  | GameOver;

type state = {
  deck: Deck.t,
  board: list(Card.t),
  p1Hand: Player.hand,
  p2Hand: Player.hand,
  p3Hand: Player.hand,
  p4Hand: Player.hand,
  p1Tricks: list(Trick.t),
  p2Tricks: list(Trick.t),
  p3Tricks: list(Trick.t),
  p4Tricks: list(Trick.t),
  maybeTrumpCard: option(Card.t), /* using slot suffix to denote an optional prop. */
  maybeLeadCard: option(Card.t),
  me: Player.id,
  dealer: Player.id,
  leader: Player.id,
  maybePlayerTurn: option(Player.id),
  team1Points: int,
  team2Points: int,
  canBeg: bool,
  canStand: bool,
  canDeal: bool,
  canDealMore: bool,
  canGiveOne: bool,
  maybeTeamHigh: option(Team.id),
  maybeTeamLow: option(Team.id),
  maybeTeamJack: option((Team.id, award)),
  maybeTeamGame: option(Team.id),
  phase,
};

let updateHand: (Player.id, Hand.t, state) => state =
  (player, hand, state) => {
    switch (player) {
    | P1 => {...state, p1Hand: hand}
    | P2 => {...state, p2Hand: hand}
    | P3 => {...state, p3Hand: hand}
    | P4 => {...state, p4Hand: hand}
    };
  };

let indexToPlayerId =
  fun
  | 0 => Player.P1
  | 1 => P2
  | 2 => P3
  | 3 => P4
  | n =>
    failwith(
      "indexToPlayer expects a number in range [0, 3] but got: "
      ++ string_of_int(n),
    );

let isGameOverTest = state => {
  state.team1Points >= 14 || state.team2Points >= 14;
};

let addPoints = (team, value, state) => {
  switch (team) {
  | Team.T1 => {...state, team1Points: state.team1Points + value}
  | T2 => {...state, team2Points: state.team2Points + value}
  };
};
