/** 
  My extensions for existing modules such as Js.Nullable.
*/

module Nullable = {
  let getUnsafe = nullable => {
    switch (Js.Nullable.toOption(nullable)) {
    | None => failwith("Nullable.getUnsafe expected a non-null value but got null.")
    | Some(value) => value
    };
  };
};

module Global = {
  let clearMaybeTimeout =
    fun
    | None => ()
    | Some(timeoutId) => Js.Global.clearTimeout(timeoutId);
};

module React = {

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
          let callback = React.Ref.current(callbackRef)
          callback();
        };

        switch(maybeDelay){
        | None => None
        | Some(delay) => 
          let id = Js.Global.setInterval(tick, delay);
          Some(() => {
            Js.Global.clearInterval(id);
          })
        }
      },
      switch(maybeDelay){
      | None => [|Js.Nullable.null|]
      | Some(delay) => [|Js.Nullable.return(delay)|]
      }
    )
  };
};
