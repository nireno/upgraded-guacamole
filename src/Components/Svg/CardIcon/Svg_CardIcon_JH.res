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
      d="M22 2v3.34h-3.129v18.02s-.016 4.337-.221 4.883c-2.413 7.9-15.771 7.35-16.436.303-.282-1.2-.204-5.186-.204-5.186h3.534s.025 1.27.073 2.21c-.457 7.483 8.996 5.929 9.465.903.196-.902.216-3.113.216-3.113V5.34h-3.65V2z"
      style={ReactDOM.Style.make(~fill="#d40000", ~fillOpacity="1", ())}
    />
    <path
      d="M6.89 38.033c3.03-.2 4.609 2.387 4.917 4.426.184.324.284.233.355-.018.149-2.618 2.492-4.758 4.662-4.465 3.532.045 5.678 3.976 5.075 7.302-.738 4.064-2.926 6.51-4.584 9.03-1.875 2.61-4.399 6.378-5.212 8.192 0 0-2.565-3.903-5.902-8.494-2.231-3.07-4.017-6.396-4.184-9.26-.19-3.273 1.154-6.206 4.873-6.713Z"
      style={ReactDOM.Style.make(~fill="#d40000", ())}
    />
  </svg>
}
