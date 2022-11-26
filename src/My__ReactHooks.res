@ocaml.doc("
    Hook into setInterval.
    The delay is an option type. Passing None for the delay will cause the
    interval timer to effectively pause.
    See [Making setInterval Declarative](https://overreacted.io/making-setinterval-declarative-with-react-hooks/)
  ")
let useInterval = (nextCallback, maybeDelay) => {
  let callbackRef = React.useRef(() => ())

  // Remember the latest callback.
  React.useEffect1(() => {
    callbackRef.current = nextCallback
    None
  }, [nextCallback])

  // Set up the interval.
  React.useEffect1(
    () => {
      let tick = () => {
        let callback = callbackRef.current
        callback()
      }

      switch maybeDelay {
      | None => None
      | Some(delay) =>
        let id = Js.Global.setInterval(tick, delay)
        Some(() => Js.Global.clearInterval(id))
      }
    },
    switch maybeDelay {
    | None => [Js.Nullable.null]
    | Some(delay) => [Js.Nullable.return(delay)]
    },
  )
}

/* clone of reason-react-update github.com/bloodyowl/reason-react-update/blob/master/src/ReactUpdate.re */

open Belt

type rec update<'action, 'state> =
  | NoUpdate
  | Update('state)
  | UpdateWithSideEffects('state, self<'action, 'state> => option<unit => unit>)
  | SideEffects(self<'action, 'state> => option<unit => unit>)
and self<'action, 'state> = {
  send: 'action => unit,
  state: 'state,
}
and fullState<'action, 'state> = {
  state: 'state,
  sideEffects: ref<array<self<'action, 'state> => option<unit => unit>>>,
}

let useReducer = (initialState, reducer) => {
  let ({state, sideEffects}, send) = React.useReducer(({state, sideEffects} as fullState, action) =>
    switch reducer(action, state) {
    | NoUpdate => fullState
    | Update(state) => {...fullState, state}
    | UpdateWithSideEffects(state, sideEffect) => {
        state,
        sideEffects: ref(Array.concat(sideEffects.contents, [sideEffect])),
      }
    | SideEffects(sideEffect) => {
        ...fullState,
        sideEffects: ref(Array.concat(fullState.sideEffects.contents, [sideEffect])),
      }
    }
  , {state: initialState, sideEffects: ref([])})
  React.useEffect1(() =>
    if Array.length(sideEffects.contents) > 0 {
      let cancelFuncs = Array.keepMap(sideEffects.contents, func => func({state, send}))
      sideEffects := []
      Array.length(cancelFuncs) > 0 ? Some(() => cancelFuncs->Array.forEach(func => func())) : None
    } else {
      None
    }
  , [sideEffects])
  (state, send)
}
