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
      d="M2 34v-2.736h2.668L10.35 2h3.233l6.262 29.264H22V34h-7.34v-2.736h1.983l-.79-4.485h-7.41l-.571 4.485h1.983V34zm13.218-10.413L12 7.92 8.984 23.587Z"
      style={ReactDOM.Style.make(~fill="#000", ~fillOpacity="1", ())}
    />
    <path
      d="M11.109 55.017c.006-.848-.664-.864-.657-.246-.056 3.11-2.685 4.64-4.595 4.277-2.944-.56-3.884-3.483-3.856-5.204.086-5.314 5.435-9.232 10.261-16.043 3.536 6.147 9.374 10.977 9.719 15.73.46 6.318-7.537 8.675-8.678 1.31-.074-.52-.687-.753-.64-.027.175 2.018-.072 2.86 2.205 7.834H8.916c1.318-2.325 2.103-5.179 2.193-7.631z"
    />
  </svg>
}
