module Fixtures = struct
  module Abilities = struct
    let factory
        ?(breakthrough=false)
        ?(charge=false)
        ?(drain=false)
        ?(guard=false)
        ?(lethal=false)
        ?(ward=false)
        ()
      =
      { Legends.Abilities.breakthrough
      ; charge
      ; drain
      ; guard
      ; lethal
      ; ward
      }
  end

  module Card = struct
    let factory
        ?(number=1)
        ?(id=1)
        ?(type_=Legends.Card_type.Creature)
        ?(cost=1)
        ?(attack=0)
        ?(defense=0)
        ?(abilities=Abilities.factory ())
        ?(own_health_change=0)
        ?(opponent_health_change=0)
        ?(draw=0)
        ()
      =
      { Legends.Card.number
      ; id
      ; type_
      ; cost
      ; attack
      ; defense
      ; abilities
      ; own_health_change
      ; opponent_health_change
      ; draw
      }
  end

  module Board_card = struct
    let factory
        ?(card=Card.factory ())
        ?(can_attack=false)
        ()
      =
      { Legends.Board_card.card
      ; can_attack
      }
  end

  module Self = struct
    let factory
        ?(health=30)
        ?(mana=0)
        ?(deck=0)
        ?(rune=25)
        ?(board=[])
        ?(hand=[])
        ()
      =
      { Legends.Self.health
      ; mana
      ; deck
      ; rune
      ; board
      ; hand
      }
  end

  module Opponent = struct
    let factory
        ?(health=30)
        ?(mana=0)
        ?(deck=0)
        ?(rune=25)
        ?(board=[])
        ?(hand_length=0)
        ()
      =
      { Legends.Opponent.health
      ; mana
      ; deck
      ; rune
      ; board
      ; hand_length
      }
  end

  module Simulator = struct
    let factory
      ?(self=Self.factory ())
      ?(opponent=Opponent.factory ())
      ()
      =
      { Legends.Simulator.self
      ; opponent
      }
  end
end

module Simulator = struct
  let test_next_no_guard () =
    let state =
      Fixtures.Simulator.factory
        ~self:
          ( Fixtures.Self.factory
              ~board:
                [ Fixtures.Board_card.factory
                    ~card:(Fixtures.Card.factory ~id:10 ())
                    ~can_attack:true
                    ()
                ]
              ()
          )
        ~opponent:
          ( Fixtures.Opponent.factory
              ~board:
                [ Fixtures.Card.factory
                    ~id:20
                    ~abilities:(Fixtures.Abilities.factory ~guard:false ())
                    ()
                ]
              ()
          )
        ()
    in

    let _result = Legends.Simulator.next state in

    ()

  let test =
      [ ("next_ok", `Quick, test_next_no_guard)
      ]
end

let () =
  Alcotest.run
    "Legends"
    [ ("Simulator", Simulator.test)
    ]
