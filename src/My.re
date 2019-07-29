/** 
  My extensions for existing modules such as Js.Nullable.
*/

module Nullable = {
  let getUnsafe = nullable => {
    switch (Js.Nullable.toOption(nullable)) {
    | None => failwith("Nullable.getUnsafe expected a non-null value but got null.")
    | Some(value) => value
    };
  };
};

module Global = {
  let clearMaybeTimeout =
    fun
    | None => ()
    | Some(timeoutId) => Js.Global.clearTimeout(timeoutId);
};

module React = {
  include My__ReactHooks;
};

module Option = {
  let all2 = (a, b) => {
    switch (a) {
    | None => 
      None
    | Some(a) =>
      switch (b) {
      | None =>
        None
      | Some(b) => Some((a, b))
      }
    };
  };

  let all3 = (a, b, c) => {
    switch (all2(a, b)) {
    | None => None
    | Some((a, b)) =>
      switch (c) {
      | None => 
        None
      | Some(c) => Some((a, b, c))
      }
    };
  };

  let all4 = (a, b, c, d) => {
    switch (all3(a, b, c)) {
    | None => None
    | Some((a, b, c)) =>
      switch (d) {
      | None => 
        None
      | Some(d) => Some((a, b, c, d))
      }
    };
  };

  let all5 = (a, b, c, d, e) => {
    switch (all4(a, b, c, d)) {
    | None => None
    | Some((a, b, c, d)) =>
      switch (e) {
      | None => 
        None
      | Some(e) => Some((a, b, c, d, e))
      }
    };
  };

  let all6 = (a, b, c, d, e, f) => {
    switch(all5(a, b, c, d, e)){
    | None => None
    | Some((a, b, c, d, e)) =>
      switch(f){ 
      | None => 
        None
      | Some(f) => Some((a, b, c, d, e, f))
      }
    }
  }
};
