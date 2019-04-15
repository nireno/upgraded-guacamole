type context = {
  .
  "deck": list(Card.t),
  "maybeInitialTrumpCard": option(Card.t),
  "maybeTrumpCard": option(Card.t),
};

let trueCond: (context, Xstate.event) => bool = (_context, _event) => true;

let canRunPack = (context: context, _event) =>
  List.length(context##deck) >= 12;

let didTrumpSuitChange: (context, Xstate.event) => bool =
  (context, _event) =>
    switch (context##maybeInitialTrumpCard) {
    | None => false
    | Some({Card.suit: initialTrumpSuit}) =>
      switch (context##maybeTrumpCard) {
      | None => false
      | Some({Card.suit: trumpSuit}) => trumpSuit != initialTrumpSuit
      }
    };

let resetBoard = Xstate.assign(
  {
    "deck": (_context, _event) => Deck.make() |> Deck.shuffle,
    "p1Hand": (_context, _event) => [],
    "p2Hand": (_context, _event) => [],
    "p3Hand": (_context, _event) => [],
    "p4Hand": (_context, _event) => [],
  });

let make: context => Xstate.machine =
  context => {
    let machineConfig = {
      "id": "allfours",
      "initial": "newRound",
      "states": {
        "newRoundTransient": {
          "on":
            Js.Dict.fromList([
              ("", [|{"action": "resetBoard", "target": "deal"}|]),
            ]),
        },
        "deal": {
          "on": {
            "DEAL": {
              "actions": "dealCards",
              "target": "beg",
            },
          },
        },
        "beg": {
          "on": {
            "BEG": {
              "actions": "beg",
              "target": "begResponse",
            },
            "STAND_UP": {
              "actions": "standUp",
              "target": "play",
            },
          },
        },
        "begResponse": {
          "on": {
            "GIVE_ONE": {
              "actions": "giveOne",
              "target": "play",
            },
            "RUN_THE_PACK": [|{"target": "runPackTransient"}|],
          },
        },
        "runPackTransient": {
          "on":
            Js.Dict.fromList([
              (
                "",
                [|
                  {"cond": "didTrumpSuitChange", "target": "play"},
                  {"cond": "canRunPack", "target": "runPack"},
                  {"cond": "trueCond", "target": "newRoundTransient"},
                |],
              ),
            ]),
        },
        "runPack": {
          "on": {
            "RUN_THE_PACK": [|
              {"actions": "runPack", "target": "runPackTransient"},
            |],
          },
        },
        // "on": {
        //   "RUN_THE_PACK": {
        //     "actions": "runPack"
        //   }
        // }
        "play": {
          "on": {
            "TOGGLE": {
              "target": "enabled",
              "actions": "toggle",
            },
          },
        },
      },
      "context": context,
    };

    let machineOptions = {
      "actions": {
        // "toggle": toggle,
      },
      // "addOne": addOne,
      "guards": {
        "didTrumpSuitChange": didTrumpSuitChange,
        "canRunPack": canRunPack,
        "trueCond": trueCond,
      },
    };
    Xstate.makeMachine(machineConfig, machineOptions);
  };
