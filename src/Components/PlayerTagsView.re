type playerTag = Dealer | Turner;

[@react.component]
let make = (~tags, ~className=?, ~style=?) => {
  <div ?className ?style>
    {switch (tags) {
     | [] =>
       <div className="player-tag__placeholder w-1/5">
         <div style={ReactDOMRe.Style.make(~width="100%", ~paddingBottom="100%", ())} />
       </div>
     | _ =>
       List.map(
         playerTag =>
           switch (playerTag) {
           | Dealer =>
             <img
               key="player-tag__dealer"
               className="player-tag__dealer__img"
               src="./static/img/emoji_pack.svg"
             />
           | Turner =>
             <img
               key="player-tag__turner"
               className="player-tag__turner__img"
               src="./static/img/emoji_diamond.svg"
             />
           },
         tags,
       )
       |> Belt.List.toArray
       |> ReasonReact.array
     }}
  </div>;
};
