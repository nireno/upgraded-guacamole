module StringMap = Belt.Map.String;
let appLogger = Pino.make({"messageKey": "_msg", "level": "debug", "_app": "allfours", "_module": "AppPrelude"});

type str_json = string;
type sock_id = string;

let teamIdtoName = (weTeamId, teamId) => {
  teamId == weTeamId ? "We" : "Dem"
};

let leftPad = (s, ~n=0, ~c="\t", ()) => {
  let tabs = Js.String.repeat(n, c); 
  Js.String.split("\n", s)
  |> Js.Array.map(line => tabs ++ line)
  |> Js.Array.joinWith("\n")
};

let leftPad1 = s => leftPad(s, ~n=1, ());

let decodeWithDefault = (decode, default, jsonString) => {
  let parseAndDecode =
    switch (decode(jsonString |> Js.Json.parseExn)) {
    | Belt.Result.Error(_error) =>
      appLogger.error2("Failed to decode json string: %s", jsonString);
      default;
    | Belt.Result.Ok(result) => result
    };

  try (parseAndDecode) {
  | _error => default
  };
};

let not = b => !b;
let identity: 'a. 'a => 'a = a => a;

type update('state, 'effect) =
  | NoUpdate('state)
  | Update('state)
  | UpdateWithSideEffects('state, list('effect))
  | SideEffects('state, list('effect))

let daysToMillis = days => days * 24 * 60 * 60 * 1000;