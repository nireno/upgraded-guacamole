type task = unit => unit
type msOfDelay = int
type msElapsed = int
type msOfStartedAtDate = float

type timeout =
  | PausedTimeout(task, msOfDelay, msElapsed)
  | RunningTimeout(Js.Global.timeoutId, task, msOfDelay, msOfStartedAtDate)

let startTimeout = (task, msOfDelay) => {
  let timeoutId = Js.Global.setTimeout(task, msOfDelay)
  RunningTimeout(timeoutId, task, msOfDelay, Js.Date.now())
}

let pauseTimeout = timeout =>
  switch timeout {
  | RunningTimeout(timeoutId, task, msOfDelay, msOfStartedAtDate) =>
    Js.Global.clearTimeout(timeoutId)
    let msElapsed = (Js.Date.now() -. msOfStartedAtDate)->int_of_float
    PausedTimeout(task, msOfDelay, msElapsed)
  | pausedTimeout => pausedTimeout
  }

let resumeTimeout = timeout =>
  switch timeout {
  | PausedTimeout(task, msOfDelay, msElapsed) =>
    RunningTimeout(
      Js.Global.setTimeout(task, msOfDelay - msElapsed),
      task,
      msOfDelay,
      Js.Date.now(),
    )
  | runningTimeout => runningTimeout
  }

let restartTimeout = timeout => {
  let (task, msOfDelay) = switch timeout {
  | RunningTimeout(timeoutId, task, msOfDelay, _msOfStartedAtDate) =>
    Js.Global.clearTimeout(timeoutId)
    (task, msOfDelay)
  | PausedTimeout(task, msOfDelay, _msElapsed) => (task, msOfDelay)
  }
  startTimeout(task, msOfDelay)
}

let clearTimeout = timeout =>
  switch timeout {
  | RunningTimeout(timeoutId, _, _, _) => Js.Global.clearTimeout(timeoutId)
  | _pausedTimeout => ()
  }
