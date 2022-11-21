open AppPrelude

include BsSocketio.Server.Make(SocketMessages)
module Ns = BsSocketio.Namespace.Make(SocketMessages)

let logger = appLogger.makeChild({"_context": "SocketStore"})

module Handshake = {
  type query = {"clientVersion": Js.undefined<string>}
  type handshake = {"query": Js.undefined<query>}

  let isHandshakeOk = (handshake, ~clientVersion) =>
    switch handshake["query"]->Js.Undefined.toOption {
    | None => false
    | Some(query) =>
      switch query["clientVersion"]->Js.Undefined.toOption {
      | None => false
      | Some(version) => clientVersion == version
      }
    }
}

module Store: {
  type msg =
    | AddSocket(BsSocketio.Server.socketT)
    | RemoveSocket(BsSocketio.Server.socketT)

  let dispatch: msg => unit
  let getState: unit => StringMap.t<BsSocketio.Server.socketT>
} = {
  let store = ref(StringMap.empty)

  type msg =
    | AddSocket(BsSocketio.Server.socketT)
    | RemoveSocket(BsSocketio.Server.socketT)

  let dispatch = msg => {
    let sockets = store.contents
    switch msg {
    | AddSocket(socket) =>
      let sock_id = socket->Socket.getId
      store := sockets->StringMap.set(sock_id, socket)
    | RemoveSocket(socket) => store := sockets->StringMap.remove(socket->Socket.getId)
    }
  }

  let getState = () => store.contents
}

let emit = (socketMessage, sock_id) => {
  let sockets = Store.getState()
  switch sockets->StringMap.get(sock_id) {
  | None => logger.warn(`"Socket ${sock_id} not found"`)
  | Some(socket) => socket->Socket.emit(socketMessage)
  }
}
