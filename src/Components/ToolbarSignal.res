@react.component
let make = (~onClick, ~img) =>
  <div onClick className="toolbar__button">
    <div
      className="frame1x1 flex flex-row justify-center items-center relative"
      style={ReactDOM.Style.make(~paddingBottom="100%", ())}>
      <img
        className="absolute top-0"
        src=img
        style={ReactDOM.Style.make(~width="auto", ~height="100%", ~padding="8%", ())}
      />
    </div>
  </div>
