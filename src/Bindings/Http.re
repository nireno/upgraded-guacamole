  type http;
  [@bs.module "http"] external create : Express.App.t => http = "Server";
  [@bs.send] external listen : (http, int, unit => unit) => unit = "";
