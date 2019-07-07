[@bs.val] [@bs.scope "localStorage"] external getItem: string => Js.Nullable.t(string) = "";
[@bs.val] [@bs.scope "localStorage"] external setItem: (string, string) => unit = "";

/**
 Sticking to a polymorphic variant to represent LocalStorage keys provides a
 bit more type safety. for getting and setting keys. See:
 https://bucklescript.github.io/docs/en/generate-converters-accessors#convert-between-js-string-enum-and-bs-polymorphic-variant
 */
[@bs.deriving jsConverter]
type key = [
  | `AllowSubbing
  // | [@bs.as "miniCoconut"] `Kiwi
];

let getClientSettings = () =>
  ClientSettings.{
    allowSubbing:
      switch (
        getItem(keyToJs(`AllowSubbing))
        |> Js.Nullable.toOption
        |> Js.Option.getWithDefault("yes")
      ) {
      | "yes" => true
      | _ => false
      },
  };

let updateClientSettings = (newSettings) => {
  setItem(keyToJs(`AllowSubbing), newSettings.ClientSettings.allowSubbing ? "yes" : "no");
}
