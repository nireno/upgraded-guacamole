@react.component
let make = (~initials, ~seed, ~style) =>
  <div className="w-full flex flex-col">
    <img
      src={LibAvatar.getAvatarUri(~client_id=seed, ~client_profile_type=style)}
      className="w-full border border-gray-300 rounded-t bg-white"
      style={ReactDOM.Style.make(~margin="auto", ())}
    />
    <svg
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink"
      style={ReactDOM.Style.make(~isolation="isolate", ~background="#FFF", ())}
      viewBox="0 0 24 8"
      version="1.1">
      <text
        x="50%"
        y="50%"
        style={ReactDOM.Style.make(
          ~lineHeight="1",
          ~fontFamily="-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,'Noto Sans',sans-serif",
          ~fontSize="8px",
          (),
        )}
        dominantBaseline="central"
        fill="#000"
        textAnchor="middle">
        {React.string(initials)}
      </text>
    </svg>
  </div>
