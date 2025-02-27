@ocaml.doc(" I wanted to use webpack-dev-middleware in hopes that I could get hot
    module reloading to work but I don't think the ecosystem is developed
    enough to support this yet. ")
type compiler

@deriving(abstract)
type pathConfig = {publicPath: string}

@deriving(abstract)
type configuration = {output: pathConfig}

@module external webpack: configuration => compiler = "webpack"
