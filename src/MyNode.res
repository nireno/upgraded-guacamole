module Fs = {
  type callback<'data> = (Js.Nullable.t<Js.Exn.t>, 'data) => unit

  @module("fs") external readFile: (string, string, callback<string>) => unit = "readFile"

  let readFile = NodeUtil.promisify2(readFile)
}
