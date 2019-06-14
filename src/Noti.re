/** For notification messages such as "player left the game" */

[@decco]
type level = Success | Info | Warning | Danger;

[@decco]
type kind = Confirm | Duration(int);

[@decco]
type roundSummary = {
  noti_maybeTeamHigh: option(Team.id),
  noti_maybeTeamLow: option(Team.id),
  noti_maybeTeamJack: option((Team.id, GameAward.award)),
  noti_maybeTeamGame: option(Team.id)
};

[@decco]
type message = Text(string) | RoundSummary(roundSummary);

[@decco]
type t = {
  noti_id: string,
  noti_recipient: Player.id,
  noti_level: level,
  noti_message: message,
  noti_kind: kind,
};

/** 
  Helper for broadcasting information from one player to all other players.
*/
let playerBroadcast = (~from: Player.id, ~msg, ~level=Info, ~kind=Duration(3375), ()) => {
  List.filter(playerId => playerId != from, [Player.P1, P2, P3, P4])
  |> List.map(playerId =>
       {
         noti_id: Nanoid.nanoid(), 
         noti_recipient: playerId, 
         noti_message: msg, 
         noti_level: level,
         noti_kind: kind
       }
     );
};

let broadcast = (~msg, ~level=Info, ~kind=Duration(3375), ()) => 
  [Player.P1, P2, P3, P4] 
  |> List.map(playerId =>
       {
         noti_id: Nanoid.nanoid(), 
         noti_recipient: playerId, 
         noti_message: msg, 
         noti_level: level,
         noti_kind: kind
       }
     );
