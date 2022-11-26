@decco
type id =
  | T1
  | T2

type t = {
  number: id,
  points: int,
}

let stringOfTeam = x =>
  switch x {
  | T1 => "Team 1"
  | T2 => "Team 2"
  }

@react.component
let make = (~points) => {
  <div> {React.string("Points: " ++ string_of_int(points))} </div>
}
