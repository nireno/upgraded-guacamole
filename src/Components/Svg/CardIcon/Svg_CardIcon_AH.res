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
      style={ReactDOM.Style.make(~fill="#d40000", ~fillOpacity="1", ())}
    />
    <path
      d="M6.89 38.033c3.03-.2 4.609 2.387 4.917 4.426.184.324.284.233.355-.018.149-2.618 2.492-4.758 4.662-4.465 3.532.045 5.678 3.976 5.075 7.302-.738 4.064-2.926 6.51-4.584 9.03-1.875 2.61-4.399 6.378-5.212 8.192 0 0-2.565-3.903-5.902-8.494-2.231-3.07-4.017-6.396-4.184-9.26-.19-3.273 1.154-6.206 4.873-6.713Z"
      style={ReactDOM.Style.make(~fill="#d40000", ())}
    />
  </svg>
}
