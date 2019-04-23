type configuration;
type compiler;


[@bs.module] external webpack: configuration => compiler = "";
[@bs.module] external webpackDevMiddleware: compiler => Express.Middleware.t = "webpack-dev-middleware";
