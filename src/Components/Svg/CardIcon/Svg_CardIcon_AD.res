@react.component
let make = () => {
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 64" preserveAspectRatio="xMidYMid meet">
    <path
      d="M0 0h24v64H0z"
      style={ReactDOM.Style.make(
        ~opacity="1",
        ~fill="#fff",
        ~fillOpacity="1",
        ~fillRule="evenodd",
        ~stroke="none",
        ~strokeWidth="1",
        ~strokeLinecap="round",
        ~strokeLinejoin="round",
        ~strokeMiterlimit="4",
        ~strokeDasharray="none",
        ~strokeOpacity="1",
        (),
      )}
    />
    <path
      d="M6712.477-2372.264c4.064-3.966 6.733-8.29 9.286-12.449 2.238 4.576 5.45 8.58 8.924 12.449-3.716 3.838-6.597 7.941-8.913 12.256-2.568-4.423-5.27-8.427-9.297-12.256z"
      style={ReactDOM.Style.make(~fill="#d40000", ())}
      transform="translate(-6709.755 2422.585)"
    />
    <path
      d="M2.26 32.929v-2.694h2.598l5.535-28.812h3.15l6.099 28.812h2.099v2.694h-7.15v-2.694h1.931l-.77-4.415H8.536l-.557 4.415h1.932v2.694zm12.875-10.252L12 7.253 9.063 22.677Z"
      style={ReactDOM.Style.make(~fill="#d40000", ~fillOpacity="1", ())}
    />
  </svg>
}
