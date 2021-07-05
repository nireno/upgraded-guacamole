
let socket = ClientSocket.T.createWithUrl("/?clientVersion=0.6.0");
let socketContext = React.createContext(socket);

let make = React.Context.provider(socketContext);

/** Tell bucklescript how to translate props into JS */
let makeProps = (~value: ClientSocket.T.t, ~children, ()) => {
  "value": value,
  "children": children,
};