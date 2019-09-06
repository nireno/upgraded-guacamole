// This helps with delivering notifications
// I only want to deliver to `Connected` clients
let mapNotiToSocketMaybe = (gameState, noti) => {
  switch (gameState.Game.clients->Quad.get(noti.Noti.noti_recipient, _)) {
  | Connected(client) => Some({ServerEffect.sock_id: client.client_socket_id, toast: noti})
  | _ => None
  };
};
