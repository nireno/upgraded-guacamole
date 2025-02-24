type asyncCallback<'err, 'result> = (('err, 'result) => unit) => unit
type asyncPromise<'result> = unit => Js.Promise.t<'result>
@module("util")
external promisify: asyncCallback<'e, 'r> => asyncPromise<'r> = "promisify"

type asyncCallback1<'a, 'err, 'result> = ('a, ('err, 'result) => unit) => unit
type asyncPromise1<'a, 'result> = 'a => Js.Promise.t<'result>
@module("util")
external promisify1: asyncCallback1<'a, 'e, 'r> => asyncPromise1<'a, 'r> = "promisify"

type asyncCallback2<'a, 'b, 'err, 'result> = ('a, 'b, ('err, 'result) => unit) => unit
type asyncPromise2<'a, 'b, 'result> = ('a, 'b) => Js.Promise.t<'result>
@module("util")
external promisify2: asyncCallback2<'a, 'b, 'e, 'r> => asyncPromise2<'a, 'b, 'r> = "promisify"

@module("util") external inspect: 'a => string = "inspect"
