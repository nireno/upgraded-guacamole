type t = {
  nCardsToDeal: int,
  nCardsToRun: int,
  winningScore: int,
  kickPlayerMillis: int,
  gameStartingCountdownSeconds: int,
}

let default = {
  nCardsToDeal: 6,
  nCardsToRun: 3,
  winningScore: 14,
  kickPlayerMillis: 60 * 1000,
  gameStartingCountdownSeconds: 5,
}

let debug = {
  ...default,
  nCardsToDeal: 2,
  nCardsToRun: 1,
  winningScore: 4,
}

let debugGameOver = {
  ...default,
  nCardsToDeal: 1,
  nCardsToRun: 1,
  winningScore: 1,
}

let debugKickInactivePlayer = {
  ...default,
  kickPlayerMillis: 5 * 1000,
}

let fromString = x =>
  switch x {
  | "debug" => debug
  | "debugGameOver" => debugGameOver
  | "debugKick" => debugKickInactivePlayer
  | _ => default
  }
