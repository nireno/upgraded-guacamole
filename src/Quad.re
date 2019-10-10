[@decco]
type t('a) = ('a, 'a, 'a, 'a);

[@decco]
type id =
  | N1
  | N2
  | N3
  | N4;

let nextId =
  fun
  | N1 => N2
  | N2 => N3
  | N3 => N4
  | N4 => N1;

let prevId =
  fun
  | N1 => N4
  | N2 => N1
  | N3 => N2
  | N4 => N1;

let stringifyId =
  fun
  | N1 => "N1"
  | N2 => "N2"
  | N3 => "N3"
  | N4 => "N4";

let withId = ((a, b, c, d)) => ((N1, a), (N2, b), (N3, c), (N4, d));

let get = (id, quad) => {
  let (r1, r2, r3, r4) = quad;
  switch (id) {
  | N1 => r1
  | N2 => r2
  | N3 => r3
  | N4 => r4
  };
};

/** Get the first element that satisfies the predicate */
let getWhere = (f, (r1, r2, r3, r4)) => {
  f(r1) ? Some(r1) : f(r2) ? Some(r2) : f(r3) ? Some(r3) : f(r4) ? Some(r4) : None
}

// An alias to getWhere with the params switched
let find = (quad, pred) => getWhere(pred, quad);

/* Maybe get the first element that satasfies any one of the predicates applied in the
   order they were listed. 
   `(2,4,6,8)->withId->findByList([is6WithId, is5WithId, is4WithId])` gives `Some((N2, 4))` */
let rec findByList = (quad, fs) =>
  switch (fs) {
  | [] => None
  | [f, ...fs] =>
    switch (quad->find(f)) {
    | None => quad->findByList(fs)
    | Some(result) => Some(result)
    }
  };

let getPairWhere = (f, (r1, r2, r3, r4)) => {
  f(r1) ? Some((N1, r1 )) : f(r2) ? Some(( N2, r2 )) : f(r3) ? Some(( N3, r3 )) : f(r4) ? Some(( N4, r4 )) : None
}

let select = (id, f, quad) => {
  get(id, quad) |> f;
};

let put = (id, r', quad) => {
  let (r1, r2, r3, r4) = quad;
  switch (id) {
  | N1 => (r', r2, r3, r4)
  | N2 => (r1, r', r3, r4)
  | N3 => (r1, r2, r', r4)
  | N4 => (r1, r2, r3, r')
  };
};

let update = (id, f, quad) => {
  put(id, get(id, quad) |> f, quad);
};

let map = (f, (r1, r2, r3, r4)) => {
  (f(r1), f(r2), f(r3), f(r4));
};

let foldLeft = (reduce, (r1, r2, r3, r4)) => {
  let f = (acc, r) => reduce(acc, r);
  f(r1, r2) |> f(r3) |> f(r4);
};

let reduce = ((r1, r2, r3, r4), f, initialValue) =>
  f(r1, initialValue) |> f(r2) |> f(r3) |> f(r4);

let foldLeftUntil = (f, test, acc, (r1, r2, r3, r4)) => {
  let fns = [f(r1), f(r2), f(r3), f(r4)];
  Util.updateUntil(fns, test, acc);
};

let rotate = ((a, b, c, d)) => (b, c, d, a);

let rec rotateBy = (n, quad) => {
  n <= 0 ? quad : rotate(quad) |> rotateBy(n - 1);
};

let toList = ((a, b, c, d)) => [a, b, c, d];

let toArray = ((a, b, c, d)) => [|a, b, c, d|];

let toDict = ((a, b, c, d)) => [(N1, a), (N2, b), (N3, c), (N4, d)];

let exists = (f, (a, b, c, d)) => {
  f(a) || f(b) || f(c) || f(d)
};

let every = (pred, (a, b, c, d)) => {
  pred(a) && pred(b) && pred(c) && pred(d)
};

let forEach: (('a => unit), ('a, 'a, 'a, 'a)) => unit = (f, (r1, r2, r3, r4)) => {
  f(r1);
  f(r2);
  f(r3);
  f(r4);
};

let make = f => (N1, N2, N3, N4)->map(id => f(id), _);

let zip = ((x1, x2, x3, x4), (y1, y2, y3, y4)) => ((x1, y1), (x2, y2), (x3, y3), (x4, y4));

let countHaving = (quad, f) => 
  quad->toArray->Belt.Array.keep(f)->Belt.Array.length