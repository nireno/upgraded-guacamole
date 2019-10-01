open AppPrelude;

include BsSocket.Server.Make(SocketMessages);
module Ns = BsSocket.Namespace.Make(SocketMessages);

let logger = appLogger.makeChild({"_context": "SocketStore"});


module Handshake = {
  type query = {. "apiVersion": Js.undefined(string)};
  type handshake = {. "query": Js.undefined(query)};

  let isHandshakeOk = (handshake, ~apiVersion) => {
    switch (handshake##query->Js.Undefined.toOption) {
    | None => false
    | Some(query) =>
      switch (query##apiVersion->Js.Undefined.toOption) {
      | None => false
      | Some(version) => apiVersion == version
      }
    };
  };
};

module Store: {
  type msg =
    | AddSocket(BsSocket.Server.socketT)
    | RemoveSocket(BsSocket.Server.socketT);

  let dispatch: msg => unit;
  let getState: unit => StringMap.t(BsSocket.Server.socketT);
} = {
  let store = ref(StringMap.empty);

  type msg =
    | AddSocket(BsSocket.Server.socketT)
    | RemoveSocket(BsSocket.Server.socketT);

  let dispatch = msg => {
    let sockets = store^;
    switch (msg) {
    | AddSocket(socket) =>
      let sock_id = socket->Socket.getId;
      store := sockets->StringMap.set(sock_id, socket);
    | RemoveSocket(socket) => store := sockets->StringMap.remove(socket->Socket.getId)
    };
  };

  let getState = () => store^;
};

let emit = (socketMessage, sock_id) => {
  let sockets = Store.getState();
  switch (sockets->StringMap.get(sock_id)) {
  | None => logger.warn({j|"Socket `$sock_id` not found"|j})
  | Some(socket) => socket->Socket.emit(socketMessage)
  };
};
