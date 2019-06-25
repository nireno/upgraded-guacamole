type client;

[@bs.deriving abstract]
type clientOptions = {
  messagingSenderId: string,
};

type messaging;

[@bs.deriving abstract]
type message = {
  token: string,
  data: string,
};

// [@bs.module] external firebase: client = "firebase";
[@bs.module "firebase"] external initializeApp: (client, clientOptions) => unit = "";
[@bs.module "firebase"] external messaging: client => messaging = "";
[@bs.module "firebase"] external onMessage: (message => unit) => unit = "";
