open AppPrelude;

module SignalTransitionConf = {
  type item = (PlayerSignal.t, string);

  [@bs.deriving abstract]
  type props = {
    [@bs.optional] left: string,
    [@bs.optional] top: string,
    [@bs.optional] opacity: string,
  };

  let getKey = ((_, key)) => key;
};

module SignalTransition = ReactSpring.MakeTransition(SignalTransitionConf);

let makeAnimatedSignal = (transition: SignalTransition.transition) => {
  let (signal, key) = transition->SignalTransition.itemGet;
  let props = transition->SignalTransition.propsGet;

  let springStyle =
    switch (props->SignalTransitionConf.leftGet) {
    | None => ReactDOMRe.Style.make(~left="0", ())
    | Some(left) => ReactDOMRe.Style.make(~left, ())
    };

  let springStyle =
    switch (props->SignalTransitionConf.topGet) {
    | None => springStyle
    | Some(top) => ReactDOMRe.(Style.combine(springStyle, Style.make(~top, ())))
    };

  let springStyle =
    switch (props->SignalTransitionConf.opacityGet) {
    | None => springStyle
    | Some(opacity') =>
      ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
    };

  let filename = PlayerSignal.toFileName(signal);

  <ReactSpring.AnimatedDiv key className="signal absolute w-full" style=springStyle>
    <div className="w-full flex justify-center bg-white" 
         style={ReactDOMRe.Style.make(~paddingBottom="100%", ())}>
      <img src={j|./static/img/$filename|j} 
           className="absolute" 
           style=ReactDOMRe.Style.make(~height="100%", ~width="auto", ~padding="8%", ()) />
    </div>
  </ReactSpring.AnimatedDiv>;
};

[@react.component]
let make = (~signal=?, ~zone) => {
  let (left, top) = switch(zone){
  | South => ("-100%", "100%")
  | East => ("100%", "100%")
  | North => ("100%", "-100%")
  | West => ("-100%", "-100%")
  };
  <>
    { 
      let signals =
        switch (signal) {
        | None => [||]
        | Some(signal) =>
          let (signal, key) = signal;
          [|(signal, key)|];
        };

      let transitions =
        SignalTransition.useTransition(
          signals,
          SignalTransition.options(
            ~from=SignalTransitionConf.props(~left, ~top, ~opacity="0", ()),
            ~enter=SignalTransitionConf.props(~left="0", ~top="0", ~opacity="1", ()),
            ~leave=SignalTransitionConf.props(~opacity="0", ()),
            ~trail=100,
            (),
          ),
        );
      transitions->Belt.Array.map(makeAnimatedSignal)->ReasonReact.array;
    }
  </>;
};