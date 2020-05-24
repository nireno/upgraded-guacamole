module T = BsSocket.Client.Make(SocketMessages);

module Admin = BsSocket.Client.Make(SocketMessages.Admin);
