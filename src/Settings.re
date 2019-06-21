type t = {
  nCardsToDeal: int,
  nCardsToRun: int,
  winningScore: int,
  kickPlayerMillis: int
}

let default = {
  nCardsToDeal: 6,
  nCardsToRun: 3,
  winningScore: 14,
  kickPlayerMillis: 30 * 1000
};

let debug = {
  ...default,
  nCardsToDeal: 2,
  nCardsToRun: 1,
  winningScore: 4,
};

let debugGameOver = {
  ...default,
  nCardsToDeal: 1,
  nCardsToRun: 1,
  winningScore: 1,
};

let debugKickInactivePlayer = {
  ...default,
  kickPlayerMillis: 5 * 1000
};

let fromString = fun
| "debug" => debug
| "debugGameOver" => debugGameOver
| "debugKick" => debugKickInactivePlayer
| _ => default;
