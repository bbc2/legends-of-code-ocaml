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
end

module Attack_simulator = struct
  let test_next_no_guard () =
    let state =
      { Legends.Attack_simulator.self_board =
          [ Fixtures.Board_card.factory
              ~card:(Fixtures.Card.factory ~id:10 ())
              ~can_attack:true
              ()
          ]
      ; opponent_board =
          [ Fixtures.Card.factory ~id:20 ~abilities:(Fixtures.Abilities.factory ~guard:false ()) ()
          ]
      }
    in

    let _result = Legends.Attack_simulator.next state in

    ()

  let test =
      [ ("next_ok", `Quick, test_next_no_guard)
      ]
end

let () =
  Alcotest.run
    "Legends"
    [ ("Attack_simulator", Attack_simulator.test)
    ]
