module Common = {
  @deriving(abstract)
  type config = {
    @optional mass: int,
    @optional tension: int,
    @optional friction: int,
  }

  module Presets = {
    @module("react-spring") @scope("config") @val external default: config = "default"
    @module("react-spring") @scope("config") @val external gentle: config = "gentle"
    @module("react-spring") @scope("config") @val external wobbly: config = "wobbly"
    @module("react-spring") @scope("config") @val external stiff: config = "stiff"
    @module("react-spring") @scope("config") @val external slow: config = "slow"
    @module("react-spring") @scope("config") @val external molasses: config = "molasses"
  }
}

module type TransitionConfig = {
  type item
  type props

  let getKey: item => string
}

module type Transition = {
  type item
  type props

  @deriving(abstract)
  type transition = {
    item: item,
    props: props,
    key: string,
  }

  @deriving(abstract)
  type options = {
    from: props,
    enter: props,
    leave: props,
    @optional config: Common.config,
    trail: int,
  }

  let useTransition: (array<item>, options) => array<transition>
}

module MakeTransition = (Conf: TransitionConfig): (
  Transition with type item = Conf.item and type props = Conf.props
) => {
  type item = Conf.item
  type props = Conf.props

  @deriving(abstract)
  type transition = {
    item: item,
    props: props,
    key: string,
  }

  @deriving(abstract)
  type options = {
    from: props,
    enter: props,
    leave: props,
    @optional config: Common.config,
    trail: int,
  }

  @module("react-spring")
  external useTransition: (array<item>, item => string, options) => array<transition> =
    "useTransition"

  let useTransition = (items, options) => useTransition(items, Conf.getKey, options)
}

module AnimatedDiv = {
  @react.component @module("react-spring") @scope("animated")
  external make: (
    ~id: string=?,
    ~ref: ReactDOM.domRef=?,
    ~key: string,
    ~style: ReactDOM.Style.t,
    ~className: string,
    ~children: React.element=?,
  ) => React.element = "div"
}

module AnimatedImg = {
  @react.component @module("react-spring") @scope("animated")
  external make: (
    ~key: string=?,
    ~style: ReactDOM.Style.t=?,
    ~className: string=?,
    ~src: string=?,
    ~onClick: ReactEvent.Mouse.t => unit=?,
    ~children: React.element=?,
  ) => React.element = "img"
}

@module("react-spring")
external animated: 'a => 'a = "animated"
