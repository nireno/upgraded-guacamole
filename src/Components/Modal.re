[@react.component]
let make = (~visible=false, ~children=?) => {
  let classes = visible ? " z-10 " : " opacity-0 zneg-10 ";
  <div className={"modal absolute w-full h-full top-0 left-0 flex items-center justify-center" ++ classes}>
    <div className="p-4 border border-grey-100 absolute w-1/2 bg-white rounded-sm shadow-lg flex flex-col items-center justify-center text-2xl">
      {Js.Option.getWithDefault(ReasonReact.null, children)}
    </div>
  </div>
}
