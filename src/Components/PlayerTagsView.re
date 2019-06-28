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
               className="player-tag__dealer__img player-tags__item"
               src="./static/img/emoji_pack.svg"
             />
           | Turner =>
             <React.Fragment key="player-tag__turner">
               <img
                 className="player-tag__turner__img player-tags__item"
                 src="./static/img/emoji_thinking.svg"
               />
               <CountdownView className="player-tags__item self-center text-center" from={SharedGame.settings.kickPlayerMillis / 1000} />
             </React.Fragment>;
           },
         tags,
       )
       |> Belt.List.toArray
       |> ReasonReact.array
     }}
  </div>;
};
