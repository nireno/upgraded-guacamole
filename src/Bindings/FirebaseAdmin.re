type t;
[@bs.module] external admin: t = "firebase-admin";

type credential;
[@bs.deriving abstract]
type options = {credential};

[@bs.send] external initializeApp: (t, options) => unit = "";
[@bs.send] [@bs.scope "credential"] external applicationDefault: t => credential = "";

type messenger;
[@bs.send] external messaging: t => messenger = "";

type messageId = string;
[@bs.send] external send: (messenger, Firebase.message) => Js.Promise.t(messageId) = "";
