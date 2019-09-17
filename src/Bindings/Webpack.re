/** I wanted to use webpack-dev-middleware in hopes that I could get hot
    module reloading to work but I don't think the ecosystem is developed
    enough to support this yet. */

type compiler;

[@bs.deriving abstract]
type pathConfig = {publicPath: string};

[@bs.deriving abstract]
type configuration = { output: pathConfig};

[@bs.module] external webpack: configuration => compiler = "webpack";

[@bs.module]
external webpackDevMiddleware: (compiler, 'options) => Express.Middleware.t =
  "webpack-dev-middleware";
