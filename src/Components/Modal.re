[@react.component]
let make = (~visible=false, ~children=?) => {
  let classes = visible ? " z-20 " : " -z-20 opacity-0 ";
  <div className={"modal absolute w-full h-full top-0 left-0 flex items-center justify-center" ++ classes}>
    <div className="p-4 border border-solid border-gray-200 absolute w-11/12 bg-white rounded-sm shadow-lg flex flex-col items-center justify-center rounded">
      {Js.Option.getWithDefault(ReasonReact.null, children)}
    </div>
  </div>
}
