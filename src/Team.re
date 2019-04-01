type teamNumber =
  | T1
  | T2;

type t = {
  number: teamNumber,
  points: int,
};

let component = ReasonReact.statelessComponent("Team");

let make = (props, _children) => {
  {
    ...component,
    render: _self => {
      <div>
        {ReasonReact.string("Points: " ++ string_of_int(props.points))}
      </div>;
    },
  };
};
