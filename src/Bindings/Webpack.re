type compiler;

[@bs.deriving abstract]
type pathConfig = {publicPath: string};

[@bs.deriving abstract]
type configuration = { output: pathConfig};

[@bs.module] external webpack: configuration => compiler = "";

[@bs.module]
external webpackDevMiddleware: (compiler, 'options) => Express.Middleware.t =
  "webpack-dev-middleware";
