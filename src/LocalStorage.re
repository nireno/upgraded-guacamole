open AppPrelude;
[@bs.val] [@bs.scope "localStorage"] external getItem: string => Js.Nullable.t(string) = "";
[@bs.val] [@bs.scope "localStorage"] external setItem: (string, string) => unit = "";

/**
 Sticking to a polymorphic variant to represent LocalStorage keys provides a
 bit more type safety. for getting and setting keys. See:
 https://bucklescript.github.io/docs/en/generate-converters-accessors#convert-between-js-string-enum-and-bs-polymorphic-variant
 */
[@bs.deriving jsConverter]
type key = [
  | `Volume
  | `Muted
  | `ClientId
  // | [@bs.as "miniCoconut"] `Kiwi
];

/** 
  Local storage stores string values. This function takes: 
  * a function to map the string value to a reason type,
  * a default reasonml value in case the item does not exist in local storage
  * and the item key
*/
let getItemWithDefault = (key, f, default) =>
  keyToJs(key) |> getItem |> Js.Nullable.toOption |> Belt.Option.mapWithDefault(_, default, f);



let getClientSettings = () => {
  let decodeVolume = volumeJson =>
    decodeWithDefault(ClientSettings.volume_decode, ClientSettings.defaults.volume, volumeJson);

  let volume = getItemWithDefault(`Volume, decodeVolume, ClientSettings.defaults.volume);

  let client_id =
    switch (keyToJs(`ClientId)->getItem->Js.Nullable.toOption) {
    | None =>
      // since I'm using nanoid to generate a random client_id as the default client_id
      // I need to save that default in localstorage the first time it is generated.
      setItem(keyToJs(`ClientId), ClientSettings.defaults.client_id);
      ClientSettings.defaults.client_id;
    | Some(client_id) => client_id
    };

  ClientSettings.{volume, client_id};
};

let updateClientSettings = newSettings => {
  setItem(
    keyToJs(`Volume),
    ClientSettings.volume_encode(newSettings.ClientSettings.volume) |> Js.Json.stringify,
  );

  setItem(keyToJs(`ClientId), newSettings.client_id);
};
