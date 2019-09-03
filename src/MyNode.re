module Fs = {

  type callback('data) = (Js.Nullable.t(Js.Exn.t), 'data) => unit;

  [@bs.module "fs"] external readFile: (string, string, callback(string)) => unit = "";

  let readFile = NodeUtil.promisify2(readFile);
}