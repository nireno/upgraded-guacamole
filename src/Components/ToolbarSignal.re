[@react.component]
let make = (~onClick, ~img) => {
  <div onClick className="toolbar__button relative" style={ReactDOMRe.Style.make(~width="8%", ())}>
    <div
      className="frame1x1 layer flex flex-row justify-center"
      style={ReactDOMRe.Style.make(~paddingBottom="100%", ())}>
      <img
        className="absolute"
        src=img
        style={ReactDOMRe.Style.make(~width="auto", ~height="100%", ~padding="8%", ())}
      />
    </div>
  </div>;
};
