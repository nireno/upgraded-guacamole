/** Originally /static/img/logo.svg */
@react.component
let make = () => {
  <svg
    xmlns="http://www.w3.org/2000/svg"
    xmlnsXlink="http://www.w3.org/1999/xlink"
    width="56.9"
    height="77.193"
    viewBox="0 0 53.344 72.368">
    <defs>
      <linearGradient
        xlinkHref="#main-screen-diamond-a"
        id="main-screen-diamond-c"
        x1="6177.885"
        x2="6146.637"
        y1="-2618.504"
        y2="-2560.857"
        gradientUnits="userSpaceOnUse"
      />
      <linearGradient id="main-screen-diamond-a">
        <stop offset="0" style={ReactDOM.Style.make(~stopColor="#ff7676", ~stopOpacity="1", ())} />
        <stop offset="1" style={ReactDOM.Style.make(~stopColor="#f30000", ~stopOpacity="1", ())} />
      </linearGradient>
      <linearGradient
        xlinkHref="#main-screen-diamond-a"
        id="main-screen-diamond-d"
        x1="6177.885"
        x2="6146.637"
        y1="-2618.504"
        y2="-2560.857"
        gradientUnits="userSpaceOnUse"
      />
      <filter
        id="main-screen-diamond-b"
        style={ReactDOM.Style.make(~colorInterpolationFilters="sRGB", ())}>
        <feFlood floodColor="#000" floodOpacity=".498" result="flood" />
        <feComposite in_="flood" in2="SourceGraphic" operator="in" result="composite1" />
        <feGaussianBlur in_="composite1" result="blur" stdDeviation="1" />
        <feOffset dx="-2" dy="2" result="offset" />
        <feComposite in_="SourceGraphic" in2="offset" result="composite2" />
      </filter>
    </defs>
    <path
      d="M6123.944-2590.774c9.92-9.682 16.436-20.238 22.667-30.39 5.464 11.17 13.306 20.943 21.786 30.39-9.073 9.368-16.105 19.386-21.76 29.917-6.267-10.795-12.865-20.571-22.693-29.917z"
      fill="#d40000"
      filter="url(#main-screen-diamond-b)"
      transform="translate(-6119.499 2627.195)"
    />
    <g fill="url(#main-screen-diamond-c)" fillOpacity="1" transform="translate(-6119.499 2627.195)">
      <path
        d="M6123.944-2590.774c9.92-9.682 16.436-20.238 22.667-30.39 5.464 11.17 13.306 20.943 21.786 30.39-9.073 9.368-16.105 19.386-21.76 29.917-6.267-10.795-12.865-20.571-22.693-29.917z"
        fill="url(#main-screen-diamond-d)"
        fillOpacity="1"
      />
    </g>
  </svg>
}
