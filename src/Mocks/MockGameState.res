let makeInitialPlayerData: unit => Game.playerData = () => {
  pla_hand: list{},
  pla_tricks: list{},
  pla_card: None,
}

let mockTeams: (Game.teamState, Game.teamState) = (
  {
    team_score: 0,
    team_points: 0,
  },
  {
    team_score: 0,
    team_points: 0,
  },
)

let makeMockSocketId = () => {
  let random = Random.int(1000)
  "mock-socket-id-" ++ string_of_int(random)
}

let mockClients = Quad.put(
  N1,
  Game.Vacant,
  Quad.make(_ => {
    let mockSocketId = makeMockSocketId()

    Game.Attached({
      client_socket_id: mockSocketId,
      client_username: mockSocketId,
      client_id: mockSocketId,
      client_initials: "MK",
      client_profile_type: Machine,
      client_connected_at: Js.Date.now(),
    })
  }),
)

let make: unit => Game.state = () => {
  game_id: Public("1"),
  deck: Deck.make(),
  players: Quad.make(_ => makeInitialPlayerData()),
  clients: mockClients,
  teams: mockTeams,
  maybeTrumpCard: None,
  maybeLeadCard: None,
  dealer: N1,
  leader: N2,
  maybeTeamHigh: None,
  maybeTeamLow: None,
  maybeTeamJack: None,
  phase: FindPlayersPhase({
    emptySeatCount: 1,
    canSub: false /* when there exists a public game in FindSubsPhase */,
  }),
  game_follow_suit: None,
}
