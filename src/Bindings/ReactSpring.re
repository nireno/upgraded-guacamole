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
    trail: int,
  };

  [@bs.module "react-spring"]
  external useTransition: ( array(item), item => string, options) => array(transition) = "";
  
  let useTransition = (items, options) => useTransition(items, Conf.getKey, options);
};

module AnimatedDiv = {
  [@react.component] [@bs.module "react-spring"][@bs.scope "animated"] 
  external make: (
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
external animated: 'a => 'a = ""
