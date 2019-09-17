open AppPrelude;
[@bs.val] [@bs.scope "localStorage"] external getItem: string => Js.Nullable.t(string) = "getItem";
[@bs.val] [@bs.scope "localStorage"] external setItem: (string, string) => unit = "setItem";

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
  | `ClientProfileType
  | `ClientInitials
  // | [@bs.as "miniCoconut"] `Kiwi
];

/** 
  Local storage stores string values. This function takes: 
  * a function to map the string value to a reason type,
  * a default reasonml value in case the item does not exist in local storage
  * and the item key
*/
let getItemWithDefault = (key, jsonDecoder, default) => {
  let jsonDecoder = decodeWithDefault(jsonDecoder, default);
  keyToJs(key) |> getItem |> Js.Nullable.toOption |> Belt.Option.mapWithDefault(_, default, jsonDecoder);
};

let getClientSettings = () => {
  let volume =
    getItemWithDefault(`Volume, ClientSettings.volume_decode, ClientSettings.defaults.volume);

  let client_profile_type =
    getItemWithDefault(
      `ClientProfileType,
      ClientSettings.profileType_decode,
      ClientSettings.defaults.client_profile_type,
    );

  let client_id =
    switch (keyToJs(`ClientId)->getItem->Js.Nullable.toOption) {
    | None =>
      // since I'm using nanoid to generate a random client_id as the default client_id
      // I need to save that default in localstorage the first time it is generated.
      setItem(keyToJs(`ClientId), ClientSettings.defaults.client_id);
      ClientSettings.defaults.client_id;
    | Some(client_id) => client_id
    };

  let client_initials = 
    switch (keyToJs(`ClientInitials)->getItem->Js.Nullable.toOption) {
    | None =>
      ClientSettings.defaults.client_initials;
    | Some(client_initials) => client_initials
    };
  
  ClientSettings.{volume, client_id, client_profile_type, client_initials};
};

let updateClientSettings = newSettings => {
  setItem(
    keyToJs(`Volume),
    ClientSettings.volume_encode(newSettings.ClientSettings.volume) |> Js.Json.stringify,
  );

  setItem(keyToJs(`ClientId), newSettings.client_id);

  setItem(
    keyToJs(`ClientProfileType),
    ClientSettings.profileType_encode(newSettings.ClientSettings.client_profile_type)
    |> Js.Json.stringify,
  );

  setItem(
    keyToJs(`ClientInitials),
    newSettings.ClientSettings.client_initials,
  );
};
