type machine;
type actionName = string;
type assignAction; /* return value of the assign function */
type event;

[@bs.module "xstate"]
external makeMachine: (Js.t('a), Js.t('b)) => machine = "Machine";
[@bs.module "xstate"] external assign: Js.t('a) => assignAction = "";

module type CustomMachine = {
  type context;
  let make: context => machine;
};

module type S = {
  type t;
  type context;
  type state = {. "context": context};
  let make: context => machine;
  let getInitialState: machine => state;
  let getContext: state => context;
  let transition: (machine, state, actionName) => state;
  let matches: (state, 'a) => bool;
};

module Make = (M: CustomMachine) : (S with type context = M.context) => {
  type t;
  type context = M.context;
  type state = {. "context": context};

  let make = M.make;

  [@bs.get] external getInitialState: machine => state = "initialState";
  [@bs.get] external getContext: state => context = "context";
  [@bs.send] external transition: (machine, state, actionName) => state = "";
  [@bs.send] external matches: (state, 'a) => bool = "";
};
