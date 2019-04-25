type str_json = string;
let debug = x => Js.log(x);
let debugError = debug;
let debuggin = (str, ~depth=0, ()) =>
  print_endline(Js.String.repeat(depth, " ") ++ str);

let teamOfPlayer =
  Player.(
    fun
    | P1
    | P3 => Team.T1
    | P2
    | P4 => Team.T2
  );
