open AppPrelude;
type ioPlayerId = int
type ioTeamId = int

type clientToServer =
  | IO_JoinGame(string)
  | IO_PlayCard(ioPlayerId, str_json)
  | IO_BlockPlay(ioPlayerId) //#todo Remove. blockplay doesn't make sense in client-server model.
  | IO_EndTrick
  | IO_NewRound
  | IO_EndRound
  | IO_Beg
  | IO_Stand
  | IO_GiveOne
  | IO_Deal
  | IO_RunPack
  | IO_DealAgain
  | IO_CheatPoints(ioTeamId, int);

type serverToClient =
  | SetState(str_json)

let ioOfPlayer:Player.id => int = player => 
switch(player){
  | P1 => 1
  | P2 => 2
  | P3 => 3
  | P4 => 4
}


let maybePlayerOfIO: int => option(Player.id) = int_player => {
  switch(int_player){
    | 1 => Some(P1)
    | 2 => Some(P2)
    | 3 => Some(P3)
    | 4 => Some(P4)
    | _ => None
  }
}

let maybeTeamOfIO: int => option(Team.id) = int_team => {
  switch(int_team){
    | 1 => Some(T1)
    | 2 => Some(T2)
    | _ => None
  }
}

let jsonOfCardUnsafe: Card.t => str_json = (card) => 
  card |> Card.tToJs |> Obj.magic |> Js.Json.stringify;

[@bs.scope "JSON"] [@bs.val]
external cardOfJson: string => Card.abs_t = "parse";
let cardOfJsonUnsafe = json => json |> cardOfJson |> Card.tFromJs;


/** Jsonify phase and reasonify phase use BsSocket.Json codecs to 
  massage reason's non-json-friendly nested variant constructors 
  such as FindSubsPhase(2, DealPhase) to and from json when 
  this type of data is wrapped in an object like 
  state = { phase: FindSubsPhase(2, DealPhase), ... }

  This can probably be made more "generic" by passing the field name
  to be massaged in the jsonify function.
 */

let toValidJson = BsSocket.Json.toValidJson;
let jsonifyField: (ClientGame.abs_state, string) => ClientGame.abs_state = 
  [%raw (abs_state, field) => 
    "{ abs_state[field] = toValidJson(abs_state[field]); 
      return abs_state; }"];

let fromValidJson = BsSocket.Json.fromValidJson;
let reasonifyField: (ClientGame.abs_state, string) => ClientGame.abs_state = 
  [%raw (abs_state, field) => 
    "{ abs_state[field] = fromValidJson(abs_state[field]); 
      return abs_state; }"];


let jsonOfClientGameState: ClientGame.state => str_json = (cgs) => {
  cgs |> ClientGame.stateToJs 
      |> jsonifyField(_, "phase") 
      |> jsonifyField(_, "gamePhase") 
      |> jsonifyField(_, "activePlayerPhase") 
      |> jsonifyField(_, "hand") 
      |> Obj.magic |> Js.Json.stringify
};

[@bs.scope "JSON"] [@bs.val]
external clientGameStateOfJsonUnsafe: string => ClientGame.abs_state = "parse";

let clientGameStateOfJsonUnsafe: str_json => ClientGame.state = str_json => { 
   str_json |> clientGameStateOfJsonUnsafe  
      |> reasonifyField(_, "phase") 
      |> reasonifyField(_, "gamePhase") 
      |> reasonifyField(_, "activePlayerPhase") 
      |> reasonifyField(_, "hand") 
      |> ClientGame.stateFromJs;
};


/** 
  The following deprecated code would have been a more robust solution to the problem of
  transferring data using primitive types since it does some meaningful data
  validation.
 */
// let maybeCardOfIO: str_json => option(Card.t) = (str_json) =>  {
//   let maybeRank = Card.Rank.maybeRankOfInt(int_rank);
//   let maybeSuit = Card.Suit.maybeSuitOfString(str_suit);
  
//   switch(maybeRank){
//     | None => None
//     | Some(rank) => 
//       switch(maybeSuit){
//         | None => None
//         | Some(suit) => Some(Card.{rank, suit})
//       }
//   }
  
// }
