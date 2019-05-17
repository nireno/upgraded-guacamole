[@decco]
type id =
  | T1
  | T2;

type t = {
  number: id,
  points: int,
};

let stringOfTeam =
  fun
  | T1 => "Team 1"
  | T2 => "Team 2";

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
