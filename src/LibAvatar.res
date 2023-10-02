let dicebearCollectionOfProfileType = x =>
  switch x {
  | ClientSettings.Masculine => DiceBear.Collection.avatarsMaleSprites
  | Feminine => DiceBear.Collection.avatarsFemaleSprites
  | Machine => DiceBear.Collection.bottts
  | Abstract => DiceBear.Collection.avatarsJdenticonSprites
  }

let getAvatarUri = (~client_id, ~client_profile_type) => {
  let dicebearCollection = dicebearCollectionOfProfileType(client_profile_type)

  DiceBear.createAvatar(dicebearCollection, {"seed": client_id})->DiceBear.toDataUriSync
}
