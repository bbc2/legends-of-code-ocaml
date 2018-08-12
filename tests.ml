module Fixtures = struct
  module Abilities = struct
    let factory
        ?(breakthrough=false)
        ?(charge=false)
        ?(guard=false)
        ()
      =
      { Legends.Abilities.breakthrough
      ; charge
      ; guard
      }
  end

  module Card = struct
    let factory
        ?(number=1)
        ?(id=1)
        ?(type_=0)
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
end

module Attack_simulator = struct
  let test_next_no_guard () =
    let state =
      { Legends.Attack_simulator.self_can_attack =
          [ Fixtures.Card.factory ~id:10 ()
          ]
      ; opponent_guards =
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
