/** For notification messages such as "player left the game" */

[@decco]
type kind = Success | Info | Warning | Danger;

[@decco]
type t = {
  noti_id: string,
  noti_recipient: Player.id,
  noti_kind: kind,
  noti_message: string
};

/** 
  Helper for broadcasting information from one player to all other players.
*/
let forBroadcast = (~from: Player.id, ~msg, ~kind=Info, ()) => {
  List.filter(playerId => playerId != from, [Player.P1, P2, P3, P4])
  |> List.map(playerId =>
       {
         noti_id: Nanoid.nanoid(), 
         noti_recipient: playerId, 
         noti_message: msg, 
         noti_kind: kind
       }
     );
};
