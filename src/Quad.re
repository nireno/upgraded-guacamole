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

  let stringifyId = fun
  | N1 => "N1"
  | N2 => "N2"
  | N3 => "N3"
  | N4 => "N4";


  let get = (id, quad) => {
    let (r1, r2, r3, r4) = quad;
    switch (id) {
    | N1 => r1
    | N2 => r2
    | N3 => r3
    | N4 => r4
    };
  };

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

  let foldLeft = (f, acc, (r1, r2, r3, r4)) => {
    f(r1, acc) |> f(r2) |> f(r3) |> f(r4);
  };

  let foldLeftUntil = (f, test, acc, (r1, r2, r3, r4)) => {
    let fns = [f(r1), f(r2), f(r3), f(r4)];
    Util.updateUntil(fns, test, acc);
  };


  let rotate = ((a, b, c, d)) => (b, c, d, a);

  let rec rotateBy = (n, quad) => {
    n <= 0 ? quad : rotate(quad) |> rotateBy(n - 1);
  };

  let toList = ((a, b, c, d)) => [a, b, c, d];

  let toDict = ((a, b, c, d)) => [(N1, a), (N2, b), (N3, c), (N4, d)];
