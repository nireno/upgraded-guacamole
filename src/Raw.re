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
