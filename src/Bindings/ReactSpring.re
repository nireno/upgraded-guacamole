module Common = {
  [@bs.deriving abstract]
  type config = {
      [@bs.optional] mass: int,
      [@bs.optional] tension: int,
      [@bs.optional] friction: int,
  };

  module Presets = {
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external default: config = "default";
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external gentle: config = "gentle";
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external wobbly: config = "wobbly";
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external stiff: config = "stiff";
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external slow: config = "slow";
    [@bs.module "react-spring"] [@bs.scope "config"] [@bs.val] external molasses: config = "molasses";
  };
}

module type TransitionConfig = {
  type item;
  type props;

  let getKey: item => string;
};

module type Transition = {
  type item;
  type props;

  [@bs.deriving abstract]
  type transition = {
    item: item,
    props: props,
    key: string
  };

  [@bs.deriving abstract]
  type options = {
    from: props,
    enter: props,
    leave: props,
    [@bs.optional] config: Common.config,
    trail: int,
  };

  let useTransition: 
    ( array(item),  options) => array(transition);
};

module MakeTransition = (Conf: TransitionConfig): 
    (Transition with type item = Conf.item and type props = Conf.props) => {

  type item = Conf.item;
  type props = Conf.props;

  [@bs.deriving abstract]
  type transition = {
    item: item,
    props: props,
    key: string
  };

  [@bs.deriving abstract]
  type options = {
    from: props,
    enter: props,
    leave: props,
    [@bs.optional] config: Common.config,
    trail: int,
  };

  [@bs.module "react-spring"]
  external useTransition: ( array(item), item => string, options) => array(transition) = "useTransition";
  
  let useTransition = (items, options) => useTransition(items, Conf.getKey, options);
};

module AnimatedDiv = {
  [@react.component] [@bs.module "react-spring"][@bs.scope "animated"] 
  external make: (
      ~id: string=?,
      ~ref: ReactDOMRe.domRef=?,
      ~key: string, 
      ~style: ReactDOMRe.Style.t, 
      ~className: string, 
      ~children:React.element=?) => React.element = "div"
};


module AnimatedImg = {
  [@react.component] [@bs.module "react-spring"][@bs.scope "animated"] 
  external make: (
      ~key: string=?, 
      ~style: ReactDOMRe.Style.t=?, 
      ~className: string=?, 
      ~src: string=?,
      ~onClick: ReactEvent.Mouse.t => unit=?,
      ~children: React.element=?) => React.element = "img"
};


[@bs.module "react-spring"]
external animated: 'a => 'a = "animated"
