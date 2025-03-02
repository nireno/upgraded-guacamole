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
      d="M2 34V2h3.193v32zM14.658 2c-3.074 0-5.545 2.728-5.545 6.116v19.768c0 3.388 2.47 6.116 5.545 6.116h1.792c3.074 0 5.55-2.728 5.55-6.116V8.116C22 4.728 19.524 2 16.45 2zm.924 3.025c1.818 0 3.278 1.358 3.278 3.048v20.032c0 1.691-1.461 3.053-3.277 3.053s-3.277-1.362-3.277-3.053V8.073c0-1.69 1.461-3.048 3.277-3.048z"
      style={ReactDOM.Style.make(~fill="#000", ~fillOpacity="1", ())}
    />
    <path
      d="M11.109 55.017c.006-.848-.664-.864-.657-.246-.056 3.11-2.685 4.64-4.595 4.277-2.944-.56-3.884-3.483-3.856-5.204.086-5.314 5.435-9.232 10.261-16.043 3.536 6.147 9.374 10.977 9.719 15.73.46 6.318-7.537 8.675-8.678 1.31-.074-.52-.687-.753-.64-.027.175 2.018-.072 2.86 2.205 7.834H8.916c1.318-2.325 2.103-5.179 2.193-7.631z"
    />
  </svg>
}
