type teamData<'a> = ('a, 'a)

let get = (id, rs: teamData<'a>) => {
  let (r1, r2) = rs
  switch id {
  | Team.T1 => r1
  | T2 => r2
  }
}

let select = (id, f, rs: teamData<'a>) => get(id, rs) |> f

let map = (f, (a, b): teamData<'a>) => (f(a), f(b))

let put = (id, r', rs: teamData<'a>) => {
  let (r1, r2) = rs
  switch id {
  | Team.T1 => (r', r2)
  | T2 => (r1, r')
  }
}

let update = (id, f, rs: teamData<'a>) => put(id, get(id, rs) |> f, rs)

let toList = ((a, b)) => list{a, b}

let toDict = ((a, b)) => list{(Team.T1, a), (T2, b)}
