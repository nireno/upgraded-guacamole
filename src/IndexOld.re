 // let createCheatPoints = team => {
      //   node_env != "production"
      //     ? <span>
      //         <button onClick={_event => send(CheatPoints(team, 1))}>
      //           {ReasonReact.string("+1")}
      //         </button>
      //         <button onClick={_event => send(CheatPoints(team, 6))}>
      //           {ReasonReact.string("+6")}
      //         </button>
      //         <button onClick={_event => send(CheatPoints(team, 13))}>
      //           {ReasonReact.string("+13")}
      //         </button>
      //       </span>
      //     : ReasonReact.null;
      // };

      // let playerPhase =
      //   playerPhase(
      //     state.phase,
      //     state.dealer,
      //     state.leader,
      //     state.maybePlayerTurn,
      //   );
      // <div>
      //   <div className="section columns">
      //     <div className="column">
      //       {switch (state.phase) {
      //        | GameOverPhase => GameOverPhase.createElement(self)
      //        | PackDepletedPhase =>
      //          <div>
      //            <div> {ReasonReact.string("No more cards")} </div>
      //            <button onClick={_event => send(DealAgain)}>
      //              {ReasonReact.string("Reshuffle")}
      //            </button>
      //          </div>
      //        | RoundSummaryPhase =>
      //          <div>
      //            <div>
      //              {ReasonReact.string(
      //                 switch (state.maybeTeamHigh) {
      //                 | None => "No one has high"
      //                 | Some(teamHigh) =>
      //                   Team.stringOfTeam(teamHigh) ++ " has high."
      //                 },
      //               )}
      //            </div>
      //            <div>
      //              {ReasonReact.string(
      //                 switch (state.maybeTeamLow) {
      //                 | None => "No one has low"
      //                 | Some(teamLow) =>
      //                   Team.stringOfTeam(teamLow) ++ " has low."
      //                 },
      //               )}
      //            </div>
      //            <div>
      //              {switch (state.maybeTeamJack) {
      //               | None => ReasonReact.null
      //               | Some((team, value)) =>
      //                 switch (value) {
      //                 | HangJackAward =>
      //                   <div>
      //                     {ReasonReact.string(
      //                        Team.stringOfTeam(team) ++ " hanged the jack.",
      //                      )}
      //                   </div>
      //                 | RunJackAward =>
      //                   <div>
      //                     {ReasonReact.string(
      //                        Team.stringOfTeam(team)
      //                        ++ " gets away with jack.",
      //                      )}
      //                   </div>
      //                 | _ => ReasonReact.null
      //                 }
      //               }}
      //            </div>
      //            <div>
      //              {switch (state.maybeTeamGame) {
      //               | None => ReasonReact.string("Tied for game.")
      //               | Some(teamGame) =>
      //                 ReasonReact.string(
      //                   Team.stringOfTeam(teamGame) ++ " gets game.",
      //                 )
      //               }}
      //            </div>
      //            <button onClick={_event => send(NewRound)}>
      //              {ReasonReact.string("Continue")}
      //            </button>
      //          </div>
      //        | _ => ReasonReact.null
      //        }}
      //     </div>
      //     <div className="column">
      //       <h1> {ReasonReact.string("Deck")} </h1>
      //       <div>
      //         {ReasonReact.string(string_of_int(List.length(state.deck)))}
      //       </div>
      //     </div>
      //     <div className="column">
      //       <h1>
      //         {ReasonReact.string(
      //            "Team 1 points: " ++ string_of_int(state.team1Points) ++ " ",
      //          )}
      //         {createCheatPoints(T1)}
      //       </h1>
      //       <h1>
      //         {ReasonReact.string(
      //            "Team 2 points: " ++ string_of_int(state.team2Points) ++ " ",
      //          )}
      //         {createCheatPoints(T2)}
      //       </h1>
      //     </div>
      //     <div className="column">
      //       <h1> {ReasonReact.string("Trump")} </h1>
      //       {switch (state.maybeTrumpCard) {
      //        | None => <h2> {ReasonReact.string("No trump")} </h2>
      //        | Some(kick) => <Card card=kick />
      //        }}
      //     </div>
      //     <div className="column">
      //       <h2> {ReasonReact.string("Board")} </h2>
      //       {List.length(state.board) == 0
      //          ? <div> {ReasonReact.string("No cards on the board")} </div>
      //          : <div />}
      //       <ul>
      //         {List.map(
      //            c =>
      //              <Card
      //                key={Card.stringOfCard(c)}
      //                card=c
      //                clickAction=?None
      //              />,
      //            state.board,
      //          )
      //          |> Belt.List.toArray
      //          |> ReasonReact.array}
      //       </ul>
      //     </div>
      //   </div>
      //   <div className="section columns">
      //     <div className="column">
      //       <Player
      //         id=P1
      //         sendDeal={sendActionEvent(Deal)}
      //         sendStandUp={sendActionEvent(Stand)}
      //         sendBeg={sendActionEvent(Beg)}
      //         sendGiveOne={sendActionEvent(GiveOne)}
      //         sendRunPack={sendActionEvent(RunPack)}
      //         playerPhase={playerPhase(P1)}
      //       />
      //       <Hand
      //         maybeLeadCard={state.maybeLeadCard}
      //         maybeTrumpCard={state.maybeTrumpCard}
      //         handPhase={
      //           Player.maybeIdEqual(state.maybePlayerTurn, P1)
      //             ? Hand.HandPlayPhase : Hand.HandWaitPhase
      //         }
      //         sendPlayCard={c => send(PlayCard(P1, state.p1Hand, c))}
      //         cards={state.p1Hand}
      //       />
      //     </div>
      //     <div className="column">
      //       <Player
      //         id=P2
      //         sendDeal={sendActionEvent(Deal)}
      //         sendStandUp={sendActionEvent(Stand)}
      //         sendBeg={sendActionEvent(Beg)}
      //         sendGiveOne={sendActionEvent(GiveOne)}
      //         sendRunPack={sendActionEvent(RunPack)}
      //         playerPhase={playerPhase(P2)}
      //       />
      //       <Hand
      //         maybeLeadCard={state.maybeLeadCard}
      //         maybeTrumpCard={state.maybeTrumpCard}
      //         handPhase={
      //           Player.maybeIdEqual(state.maybePlayerTurn, P2)
      //             ? Hand.HandPlayPhase : Hand.HandWaitPhase
      //         }
      //         sendPlayCard={c => send(PlayCard(P2, state.p2Hand, c))}
      //         cards={state.p2Hand}
      //       />
      //     </div>
      //     <div className="column">
      //       <Player
      //         id=P3
      //         sendDeal={sendActionEvent(Deal)}
      //         sendStandUp={sendActionEvent(Stand)}
      //         sendBeg={sendActionEvent(Beg)}
      //         sendGiveOne={sendActionEvent(GiveOne)}
      //         sendRunPack={sendActionEvent(RunPack)}
      //         playerPhase={playerPhase(P3)}
      //       />
      //       <Hand
      //         maybeLeadCard={state.maybeLeadCard}
      //         maybeTrumpCard={state.maybeTrumpCard}
      //         handPhase={
      //           Player.maybeIdEqual(state.maybePlayerTurn, P3)
      //             ? Hand.HandPlayPhase : Hand.HandWaitPhase
      //         }
      //         sendPlayCard={c => send(PlayCard(P3, state.p3Hand, c))}
      //         cards={state.p3Hand}
      //       />
      //     </div>
      //     <div className="column">
      //       <Player
      //         id=P4
      //         sendDeal={sendActionEvent(Deal)}
      //         sendStandUp={sendActionEvent(Stand)}
      //         sendBeg={sendActionEvent(Beg)}
      //         sendGiveOne={sendActionEvent(GiveOne)}
      //         sendRunPack={sendActionEvent(RunPack)}
      //         playerPhase={playerPhase(P4)}
      //       />
      //       <Hand
      //         maybeLeadCard={state.maybeLeadCard}
      //         maybeTrumpCard={state.maybeTrumpCard}
      //         handPhase={
      //           Player.maybeIdEqual(state.maybePlayerTurn, P4)
      //             ? Hand.HandPlayPhase : Hand.HandWaitPhase
      //         }
      //         sendPlayCard={c => send(PlayCard(P4, state.p4Hand, c))}
      //         cards={state.p4Hand}
      //       />
      //     </div>
      //   </div>
      //   <h2> {ReasonReact.string("Tricks")} </h2>
      //   <div className="section columns">
      //     <div className="column"> {createPlayerTricks(state.p1Tricks)} </div>
      //     <div className="column"> {createPlayerTricks(state.p2Tricks)} </div>
      //     <div className="column"> {createPlayerTricks(state.p3Tricks)} </div>
      //     <div className="column"> {createPlayerTricks(state.p4Tricks)} </div>
      //   </div>
      // </div>;
