type eventListener;

let preventUnloadListener: eventListener = [%raw
  {|
    function (e) {
      // Cancel the event
      e.preventDefault();
      // Chrome requires returnValue to be set
      e.returnValue = '';
    }
  |}
];

let addUnloadListener: eventListener => unit = [%raw
  (listener) => {|
    window.addEventListener("beforeunload", listener);
  |}
];

let removeUnloadListener: eventListener => unit = [%raw
  (listener) => {|
    window.removeEventListener("beforeunload", listener);
  |}
];

let authMiddleware: (string, string) => Express.Middleware.t = [%raw
  (user, pass) => {|
    return (req, res, next) => {

      // parse login and password from headers
      const b64auth = (req.headers.authorization || '').split(' ')[1] || ''
      const [login, password] = new Buffer(b64auth, 'base64').toString().split(':')
      if(login == user && password == pass){
        next();
      } else {
        res.set('WWW-Authenticate', 'Basic')
        res.status(401).send('Authentication required.')
      }
    }
  |}
];
