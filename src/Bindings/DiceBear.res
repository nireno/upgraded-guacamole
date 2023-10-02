module Collection = {
  type t

  @module("@dicebear/collection")
  external lorelei: t = "lorelei"

  @module("@dicebear/collection")
  external bottts: t = "bottts"

  @module("@dicebear/collection")
  external openPeeps: t = "openPeeps"

  @module
  external avatarsMaleSprites: t = "@dicebear/avatars-male-sprites"

  @module
  external avatarsFemaleSprites: t = "@dicebear/avatars-female-sprites"

  @module
  external avatarsJdenticonSprites: t = "@dicebear/avatars-jdenticon-sprites"
}

type t

@module("@dicebear/core")
external createAvatar: (Collection.t, {..}) => t = "createAvatar"

@send
external toDataUriSync: t => string = "toDataUriSync"
