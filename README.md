# all-fours

Card images taken from cardsJS package. See my github for a fork of the repo.
Note that the version installed using `npm install cardsJS` has a `dist` dir
which contains optimized/minified versions of the svg files. These are the
files I copied.

# Development

## Get started
Where's the primary entry point when you hit localhost:3000?
When you hit the web server (`Server.re`) you trigger a `Express.App.useOnPath /` that serves up the `./build/index.html` that loads the main bundled javascript app at `index.js` which is the compiled and bundled output of `Index.re`.
So all the action starts at `Index.re` specifically with the ReasonReactRouter url handling `switch (List.rev(url.path))`.


# Production
## Deployment checklist
1. Bump client and server version. This should force out of date clients to require a refresh to get the latest client code.
In production we ensure that the client and server versions are in sync when the client first creates a ClientSocket which passes a query string that specifies the client's version.
So before deploying a new version of the Server code, bump the `?clientVersion={version}` query string at `Index.re` and the `~clientVersion` parameter where the SocketServer.Handshake is done (currently `Server.re`).

## Run Project

```sh
npm install
npm start
# in another tab
npm run webpack
```

After you see the webpack compilation succeed (the `npm run webpack` step), open up `build/index.html` (**no server needed!**). Then modify whichever `.re` file in `src` and refresh the page to see the changes.

**For more elaborate ReasonReact examples**, please see https://github.com/reasonml-community/reason-react-example

## Run Project with Server

To run with the webpack development server run `npm run server` and view in the browser at http://localhost:8000. Running in this environment provides hot reloading and support for routing; just edit and save the file and the browser will automatically refresh.

Note that any hot reload on a route will fall back to the root (`/`), so `ReasonReact.Router.dangerouslyGetInitialUrl` will likely be needed alongside the `ReasonReact.Router.watchUrl` logic to handle routing correctly on hot reload refreshes or simply opening the app at a URL that is not the root.

To use a port other than 8000 set the `PORT` environment variable (`PORT=8080 npm run server`).

## Build for Production

```sh
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version as well as copy `src/index.html` into `build/`. You can then deploy the contents of the `build` directory (`index.html` and `Index.js`).

If you make use of routing (via `ReasonReact.Router` or similar logic) ensure that server-side routing handles your routes or that 404's are directed back to `index.html` (which is how the dev server is set up).

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.

## Project Nomenclature
"Client" modules should only contain code that can run on the client. This
might seem obvious but its easy for some nodejs code to slip in and you end
up trying to run some code on the client that only nodejs would understand
(like accessing the filesystem).

Use "Shared" modules to make some Server code shared if it is useful and
*safe* to run on the client. Not only should it run correctly it should not
*reveal any private server information.
Similarly you can make some Client code shared if it is useful on the Server.

"Shared" modules should only contain code that can run on both client and
server. This means no code that depends on the DOM for example (client only
code) and no code that accesses the filesystem for example (server only
code).

