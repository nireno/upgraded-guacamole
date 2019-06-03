[@react.component]
let make = (~children=?) => {
 <div
   className="main-menu flex flex-col justify-center items-center w-full h-full"
   style={ReactDOMRe.Style.make(
     ~backgroundImage="url('/static/img/logo.svg')",
     ~backgroundSize="contain",
     ~backgroundPosition="center",
     ~backgroundRepeat="no-repeat",
     (),
   )}>
   {Js.Option.getWithDefault(ReasonReact.null, children)}
 </div>;
}
