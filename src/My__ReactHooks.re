/**
    Hook into setInterval.
    The delay is an option type. Passing None for the delay will cause the
    interval timer to effectively pause.
    See [Making setInterval Declarative](https://overreacted.io/making-setinterval-declarative-with-react-hooks/)
  */
let useInterval = (nextCallback, maybeDelay) => {
  let callbackRef = React.useRef(() => ());

  // Remember the latest callback.
  React.useEffect1(
    () => {
      React.Ref.setCurrent(callbackRef, nextCallback);
      None;
    },
    [|nextCallback|],
  );

  // Set up the interval.
  React.useEffect1(
    () => {
      let tick = () => {
        let callback = React.Ref.current(callbackRef);
        callback();
      };

      switch (maybeDelay) {
      | None => None
      | Some(delay) =>
        let id = Js.Global.setInterval(tick, delay);
        Some(() => Js.Global.clearInterval(id));
      };
    },
    switch (maybeDelay) {
    | None => [|Js.Nullable.null|]
    | Some(delay) => [|Js.Nullable.return(delay)|]
    },
  );
};

/* clone of reason-react-update github.com/bloodyowl/reason-react-update/blob/master/src/ReactUpdate.re */

open Belt;

type update('action, 'state) =
  | NoUpdate
  | Update('state)
  | UpdateWithSideEffects('state, self('action, 'state) => option(unit => unit))
  | SideEffects(self('action, 'state) => option(unit => unit))
and self('action, 'state) = {
  send: 'action => unit,
  state: 'state,
}
and fullState('action, 'state) = {
  state: 'state,
  sideEffects: ref(array(self('action, 'state) => option(unit => unit))),
};

let useReducer = (initialState, reducer) => {
  let ({state, sideEffects}, send) =
    React.useReducer(
      ({state, sideEffects} as fullState, action) =>
        switch (reducer(action, state)) {
        | NoUpdate => fullState
        | Update(state) => {...fullState, state}
        | UpdateWithSideEffects(state, sideEffect) => {
            state,
            sideEffects: ref(Array.concat(sideEffects^, [|sideEffect|])),
          }
        | SideEffects(sideEffect) => {
            ...fullState,
            sideEffects: ref(Array.concat(fullState.sideEffects^, [|sideEffect|])),
          }
        },
      {state: initialState, sideEffects: ref([||])},
    );
  React.useEffect1(
    () =>
      if (Array.length(sideEffects^) > 0) {
        let cancelFuncs = Array.keepMap(sideEffects^, func => func({state, send}));
        sideEffects := [||];
        Array.length(cancelFuncs) > 0
          ? Some(() => cancelFuncs->Array.forEach(func => func())) : None;
      } else {
        None;
      },
    [|sideEffects|],
  );
  (state, send);
};
