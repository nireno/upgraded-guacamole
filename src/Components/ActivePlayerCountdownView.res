let infoTextOfPhase = x =>
  switch x {
  | ClientGame.DealPhase => "Dealing"
  | RunPackPhase => "Running"
  | FlipFinalTrumpPhase => "Kicking Again"
  | PackDepletedPhase => "Redealing"

  | BegPhase => "Beg Decision"
  | GiveOnePhase => "Run Decision"

  | PlayerTurnPhase(_) => "Playing"

  | FindPlayersPhase(_)
  | FindSubsPhase(_)
  | IdlePhase(_)
  | GameOverPhase(_) => ""
  }

@react.component
let make = (~gamePhase) =>
  <div className="layer flex flex-col justify-center items-center">
    <img src="./static/img/emoji_thinking.svg" className="w-1/4" />
    <CountdownView
      className="player-tags__item self-center text-center"
      from={SharedGame.settings.kickPlayerMillis / 1000}
    />
    <div className="w-full">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        xmlnsXlink="http://www.w3.org/1999/xlink"
        viewBox="0 0 70 10"
        version="1.1">
        <text
          x="50%"
          y="50%"
          style={ReactDOM.Style.make(
            ~lineHeight="1",
            ~fontFamily="-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,'Noto Sans',sans-serif",
            ~fontSize="8px",
            (),
          )}
          dominantBaseline="central"
          fill="#000"
          textAnchor="middle">
          {React.string(infoTextOfPhase(gamePhase))}
        </text>
      </svg>
    </div>
  </div>
