type playerData('a) = ('a, 'a, 'a, 'a);

let get = (id, rs: playerData('a)) => {
  let (r1, r2, r3, r4) = rs;
  switch (id) {
  | Player.P1 => r1
  | P2 => r2
  | P3 => r3
  | P4 => r4
  };
};

let select = (id, f, rs: playerData('a)) => {
  get(id, rs) |> f
};

let map = (f, (a,b,c,d):playerData('a)) => {
  (f(a), f(b), f(c), f(d))
};

let put = (id, r', rs: playerData('a)) => {
  let (r1, r2, r3, r4) = rs;
  switch (id) {
  | Player.P1 => (r', r2, r3, r4)
  | P2 => (r1, r', r3, r4)
  | P3 => (r1, r2, r', r4)
  | P4 => (r1, r2, r3, r')
  };
};

let update = (id, f, rs: playerData('a)) => {
  put(id, get(id, rs) |> f, rs);
};

let toList = ( (a, b, c, d) ) => [a, b, c, d];

let toDict = ( (a, b, c, d) ) => [(Player.P1, a), (P2, b), (P3, c), (P4, d)];