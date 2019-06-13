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
