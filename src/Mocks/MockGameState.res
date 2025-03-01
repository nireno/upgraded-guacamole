let makeInitialPlayerData: AllFours.Hand.FaceUpHand.t => Game.playerData = hand => {
  pla_hand: hand,
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
  let deck = Deck.make()->Deck.shuffle
  let (p1Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
  let (p2Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
  let (p3Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)
  let (p4Hand, deck) = Deck.deal(SharedGame.settings.nCardsToDeal, deck)

  let playerHandsQuad = Quad.make(i =>
    switch i {
    | N1 => p1Hand
    | N2 => p2Hand
    | N3 => p3Hand
    | N4 => p4Hand
    }
  )

  {
    game_id: Public("1"),
    deck,
    players: Quad.map(makeInitialPlayerData, playerHandsQuad),
    clients: mockClients,
    teams: mockTeams,
    maybeTrumpCard: None,
    maybeLeadCard: None,
    dealer: N4,
    leader: N1,
    maybeTeamHigh: None,
    maybeTeamLow: None,
    maybeTeamJack: None,
    phase: FindSubsPhase({
      emptySeatCount: 1,
      phase: BegPhase(BegPhaseDeciding),
    }),
    game_follow_suit: None,
  }
}
