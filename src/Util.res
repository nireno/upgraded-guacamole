@ocaml.doc(" 
  Apply \"middleware\" functions fns to state until the given test condition is true
  or all functions are applied. 
  fns: list('state => 'state)
  test: 'state => bool
")
let rec updateUntil = (fns, test, state) =>
  switch fns {
  | list{} => state
  | list{fn, ...fns'} => test(state) ? state : updateUntil(fns', test, fn(state))
  }


@val @return(nullable)
external getElementById: string => option<Dom.element> = "document.getElementById"