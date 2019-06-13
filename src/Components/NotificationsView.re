module NotiTransitionConf = {
    type item = Noti.t;

    [@bs.deriving abstract]
    type props = {
      [@bs.optional] left: string,
      [@bs.optional] top: string,
      [@bs.optional] opacity: string,
    }
    
    let getKey = (noti) => noti.Noti.noti_id;
  };

module NotiTransition = ReactSpring.MakeTransition(NotiTransitionConf);

[@react.component]
let make = (~id=?, ~appRect, ~notis=[]) => {
  let height = Webapi.Dom.DomRect.height(appRect);
  let fromTop = "-" ++ Js.Float.toString(height *. 0.1) ++ "px";
  let leaveTop = fromTop;

  let notiTransitions =
    NotiTransition.useTransition(
      notis |> Belt.List.toArray,
      NotiTransition.options(
        ~from=NotiTransitionConf.props(~left="0px", ~top=fromTop, ~opacity="0", ()),
        ~enter=NotiTransitionConf.props(~left="0px", ~top="0px", ~opacity="1", ()),
        ~leave=NotiTransitionConf.props(~left="0px", ~top=leaveTop, ~opacity="0", ()),
        ~config=ReactSpring.Common.Presets.wobbly,
        ~trail=100,
        (),
      ),
    );

  let elements =
    Array.map(
      (transition: NotiTransition.transition) => {
        let noti = transition->NotiTransition.itemGet;
        let key =transition->NotiTransition.keyGet;
        let props = transition->NotiTransition.propsGet;

        let springStyle =
          switch (props->NotiTransitionConf.leftGet) {
          | None => ReactDOMRe.Style.make(~left="0", ())
          | Some(left) => 
            ReactDOMRe.Style.make(~left, ())
          };

        let springStyle =
          switch (props->NotiTransitionConf.topGet) {
          | None => ReactDOMRe.Style.make(~top="0", ())
          | Some(top) => ReactDOMRe.(Style.combine(springStyle, ReactDOMRe.Style.make(~top, ())))
          };

        let springStyle =
          switch (props->NotiTransitionConf.opacityGet) {
          | None => springStyle
          | Some(opacity') =>
            ReactDOMRe.(Style.combine(springStyle, Style.make(~opacity=opacity', ())))
          };

        <ReactSpring.AnimatedDiv
          ?id
          key
          className="notification relative bg-teal-100 border-t-4 border-teal-500 border-solid rounded-b text-teal-900 px-4 py-3 shadow-md"
          style=springStyle>
          <div className="flex items-center">
            <svg
              className="fill-current h-6 w-6 text-teal-500 mr-4"
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 20 20">
              <path
                d="M2.93 17.07A10 10 0 1 1 17.07 2.93 10 10 0 0 1 2.93 17.07zm12.73-1.41A8 8 0 1 0 4.34 4.34a8 8 0 0 0 11.32 11.32zM9 11V9h2v6H9v-4zm0-6h2v2H9V5z"
              />
            </svg>
            <div> <p className="font-bold"> {ReasonReact.string(noti.noti_message)} </p> </div>
          </div>
        </ReactSpring.AnimatedDiv>;
      },
      notiTransitions,
    );
  
  <div
    className="notifications pointer-events-none absolute w-full h-full top-0 left-0 flex flex-col items-center justify-center z-20">
    {elements |> ReasonReact.array}
  </div>;
};
