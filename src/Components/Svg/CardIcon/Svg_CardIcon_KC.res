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
      d="M8.496 60.289c1.033-2.063 2.038-3.984 2.586-6.245.362-2.133-.38-1.34-.802.057-.625 1.94-2.359 2.822-3.967 2.843-2.921.038-4.378-2.197-4.287-4.602-.318-2.53 2.313-4.32 4.298-4.183 1.493.104 2.793.888 3.58 1.498.517.267 1.07.384.3-.302-1.454-1.547-2.666-2.501-2.64-4.818.026-2.177 1.936-4.446 4.467-4.375 3.163.09 4.602 2.438 4.594 4.625-.009 1.593-.827 3.06-2.09 3.968-.817.646-.588 1.13.345.518.897-.753 1.964-1.082 3.071-1.105 3.023-.046 4.206 2.499 4.033 4.864-.162 2.215-2.1 4.496-4.651 4.238-2.279-.255-3.477-2.118-4.163-4.031-.227-.587-.585-.745-.576.129.527 4.268 2.159 6.923 2.17 6.92z"
    />
    <path
      d="M2 34v-3.41h2.94V4.96H2V2h8.154v2.96H8.028l.197 12.323L15.623 4.96h-2.884L12.772 2H22v2.96h-2.632l-5.89 10.14 5.89 15.49H22V34h-9.226v-3.195l3.46-.06-4.903-12.065-3.303 4.952v7.017h2.405V34z"
      style={ReactDOM.Style.make(~fill="#000", ~fillOpacity="1", ())}
    />
  </svg>
}
