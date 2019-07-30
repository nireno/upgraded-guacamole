open Belt;

let mapNotiToSocketMaybe = (gameState, noti) => {
  let player = Quad.get(noti.Noti.noti_recipient, gameState.Game.players);
  player.sock_id_maybe->Option.map(sock_id => {ServerEffect.sock_id, toast: noti});
};
