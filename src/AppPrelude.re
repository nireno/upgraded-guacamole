type str_json = string;
let str_tab = "\t";
let str_crlf = "\n";
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

let leftPad = (s, ~n=0, ~c="\t", ()) => {
  let tabs = Js.String.repeat(n, c); 
  Js.String.split("\n", s)
  |> Js.Array.map(line => tabs ++ line)
  |> Js.Array.joinWith("\n")
};

let leftPad1 = s => leftPad(s, ~n=1, ());
