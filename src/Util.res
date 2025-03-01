open Belt

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

/* 
  Creates a string of css classes from an array of strings.
*/
let cns = classes => {
  classes
  ->Array.map(Js.String.trim)
  ->Array.keep(c => c->Js.String.length > 0)
  ->Js.Array2.joinWith(" ")
}
