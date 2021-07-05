open AppPrelude;

[@decco]
type t = {
  playersOnline: int,
  gamesOnline: int,
};

type io = string;

let to_io: t => string =
  state => {
    state->t_encode->Js.Json.stringify;
  };

let from_io: string => option(t) =
  json =>
    try(json->Js.Json.parseExn->t_decode->option_of_result) {
    | _ => None
    };
