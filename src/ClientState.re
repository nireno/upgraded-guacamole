type t = {
  countdown: int,
  maybeCountdownIntervalId: option(Js.Global.intervalId),
};

let initial = {
  countdown: SharedGame.settings.kickPlayerMillis / 1000,
  maybeCountdownIntervalId: None,
};

type action =
  | TickCountdown
  | StartCountdown(Js.Global.intervalId)
  | StopCountdown;

let reducer = (prevState, action) => {
  switch (action) {
  | TickCountdown =>
    let nextCountdown = prevState.countdown - 1;

    // if (nextCountdown <= 0) {
      switch (prevState.maybeCountdownIntervalId) {
      | Some(intervalId) when nextCountdown <= 0 => 
        Js.Global.clearInterval(intervalId);
        {maybeCountdownIntervalId: None, countdown: 0}
      | Some(_intervalId) => {...prevState, countdown: nextCountdown}
      | None => prevState
      };

  | StartCountdown(nextIntervalId) =>
    switch (prevState.maybeCountdownIntervalId) {
    | None => {...prevState, maybeCountdownIntervalId: Some(nextIntervalId)}
    | Some(intervalId) =>
      Js.Global.clearInterval(intervalId);

      {maybeCountdownIntervalId: Some(nextIntervalId), countdown: initial.countdown};
    }

  | StopCountdown =>
    switch (prevState.maybeCountdownIntervalId) {
    | None => prevState
    | Some(intervalId) =>
      Js.Global.clearInterval(intervalId);
      {maybeCountdownIntervalId: None, countdown: initial.countdown};
    }
  };
};
