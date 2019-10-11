/** 
  My extensions for existing modules such as Js.Nullable.
*/

module List = {
  let shuffle = d => {
    Random.self_init();
    let nd = List.map(c => (Random.bits(), c), d);
    let sond = List.sort(compare, nd);
    List.map(snd, sond);
  };
};

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

  let task = (option, task) =>
    switch (option) {
    | None => ()
    | Some(data) => task(data)
    };
};

module Document = {
  type location;
  [@bs.val] [@bs.scope "document"] external location: location = "location";
};

module URL = {
  type url;
  type searchParams;
  [@bs.new] external makeURL: Document.location => url = "URL";
  [@bs.get] external searchParams: url => searchParams = "searchParams";
  [@bs.send] external getSearchParam: (searchParams, string) => Js.Nullable.t(string) = "get";

  let getSearchParam = (searchParams, param) => {
    searchParams->getSearchParam(param)->Js.Nullable.toOption;
  };
};

module StringMap = {
  /* Update the mapping, returning a pair of the (possibly unchanged) mapping 
     and a maybe of the updated value. 

     This is just a shortcut to avoid having to do an additional Belt.Map.String.get
     after performing a Belt.Map.string.update in order to access the value that may
     have been updated.
     */
  let update = (mapping, key, updateFn) =>
    switch (mapping->Belt.Map.String.get(key)) {
    | None => (mapping, None)
    | Some(value) => 
      let value' = updateFn(value);
      let mapping' = mapping->Belt.Map.String.set(key, value');
      (mapping', Some(value'))
    };
};