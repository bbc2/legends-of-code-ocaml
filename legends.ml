module Abilities = struct
  type t =
    { breakthrough : bool
    ; charge : bool
    ; drain : bool
    ; guard : bool
    ; lethal : bool
    ; ward : bool
    }
end

module Card_type = struct
  type t =
    | Creature
    | Item_blue
    | Item_green
    | Item_red
end

module Card = struct
  type t =
    { number : int
    ; id : int
    ; type_ : Card_type.t
    ; cost : int
    ; attack : int
    ; defense : int
    ; abilities : Abilities.t
    ; own_health_change : int
    ; opponent_health_change : int
    ; draw : int
    }
end

module Board_card = struct
  type t =
    { card : Card.t
    ; can_attack : bool
    }
end

module Self = struct
  type t =
    { health : int
    ; mana : int
    ; deck : int
    ; rune : int
    ; board : Board_card.t list
    ; hand : Card.t list
    }
end

module Opponent = struct
  type t =
    { health : int
    ; mana : int
    ; deck : int
    ; rune : int
    ; board : Card.t list
    ; hand_length : int
    }
end

module Round = struct
  type t =
    | Draft of
        { cards : Card.t list
        }
    | Battle of
        { self : Self.t
        ; opponent : Opponent.t
        }
end

module Action = struct
  type t =
    | Pick of
        { position : int
        }
    | Summon of
        { id : int
        }
    | Attack of
        { attacker_id : int
        ; target_id : int
        }
    | Pass
end

module Simulator = struct
  type state =
    { self : Self.t
    ; opponent : Opponent.t
    }

  let next ~action state =
    match action with
    | Action.Attack {attacker_id; target_id} ->
      let (attackers, others_can_attack) =
        state.self.Self.board
        |> List.partition (fun card -> card.Board_card.card.Card.id == attacker_id)
      in
      let attacker = List.hd attackers in
      if target_id == -1 then
        { state with
          self = { state.self with
                   board = {attacker with can_attack = false} :: others_can_attack
                 }
        }
      else
        let (targets, other_opponent_cards) =
          state.opponent.Opponent.board
          |> List.partition (fun card -> card.Card.id == target_id)
        in
        let target = List.hd targets in
        let target_defense = target.Card.defense - attacker.Board_card.card.Card.attack in
        { self =
            { state.self with board = {attacker with can_attack = false} :: others_can_attack
            }
        ; opponent =
            { state.opponent with
              board =
                if target_defense > 0 then
                  {target with defense = target_defense} :: other_opponent_cards
                else
                  other_opponent_cards
            }

        }
    | Action.Summon {id} ->
      let (summoned, hand) =
        state.self.Self.hand
        |> List.partition (fun card -> card.Card.id == id)
      in
      let summoned = List.hd summoned in
      let can_attack = summoned.Card.abilities.Abilities.charge in
      { state with
        self =
          { state.self with
            mana = state.self.Self.mana - summoned.Card.cost
          ; hand
          ; board =
              {Board_card.card = summoned; can_attack} :: state.self.Self.board
          }
      }
    | Action.Pick {position = _}
    | Action.Pass ->
      failwith "next: Unsupported action"
end

module Strategy = struct
  let compare_card_costs_decr card_0 card_1 =
    compare card_0.Card.cost card_1.Card.cost

  let summon_action state =
    let self = state.Simulator.self in
    let can_be_summoned =
      self.Self.hand
      |> List.filter (fun card -> card.Card.cost <= self.Self.mana)
      |> List.filter (fun card -> card.Card.type_ == Card_type.Creature)
    in
    match List.sort compare_card_costs_decr can_be_summoned with
    | [] ->
      None
    | most_expensive :: _ ->
      Some (Action.Summon {id = most_expensive.Card.id})

  let attack_action state =
    let can_attack =
      state.Simulator.self.Self.board
      |> List.filter (fun card -> card.Board_card.can_attack)
      |> List.map (fun card -> card.Board_card.card)
    in
    match can_attack with
    | [] ->
      None
    | attacker :: _ ->
      ( let guards =
          state.Simulator.opponent.Opponent.board
          |> List.filter (fun card -> card.Card.abilities.Abilities.guard)
        in
        match guards with
        | [] ->
          Some (Action.Attack {attacker_id = attacker.Card.id; target_id = -1})
        | guard :: _ ->
          Some (Action.Attack {attacker_id = attacker.Card.id; target_id = guard.Card.id})
      )

  let actions round =
    match round with
    | Round.Draft _ ->
      [Action.Pass]
    | Round.Battle {self; opponent} ->
      let rec act ~state ~actions =
        match summon_action state with
        | None ->
          ( match attack_action state with
            | None ->
              actions
            | Some action ->
              let new_state = Simulator.next ~action state in
              act ~state:new_state ~actions:(actions @ [action])
          )
        | Some action ->
          let new_state = Simulator.next ~action state in
          act ~state:new_state ~actions:(actions @ [action])
      in
      act
        ~state:
          { Simulator.self
          ; opponent
          }
        ~actions:[]
end

module Raw = struct
  module Player = struct
    type t =
      { health : int
      ; mana : int
      ; deck : int
      ; rune : int
      }

    let of_params health mana deck rune =
      {health; mana; deck; rune}
  end

  module Card = struct
    type t =
      { number : int
      ; id : int
      ; location : int
      ; type_ : int
      ; cost : int
      ; attack : int
      ; defense : int
      ; abilities : string
      ; own_health_change : int
      ; opponent_health_change : int
      ; draw : int
      }

    let of_params
        number
        id
        location
        type_
        cost
        attack
        defense
        abilities
        own_health_change
        opponent_health_change
        draw
      =
      { number
      ; id
      ; location
      ; type_
      ; cost
      ; attack
      ; defense
      ; abilities
      ; own_health_change
      ; opponent_health_change
      ; draw
      }

    let parse_abilities raw_abilities =
      { Abilities.breakthrough = String.contains raw_abilities 'B'
      ; charge = String.contains raw_abilities 'C'
      ; drain = String.contains raw_abilities 'D'
      ; guard = String.contains raw_abilities 'G'
      ; lethal = String.contains raw_abilities 'L'
      ; ward = String.contains raw_abilities 'W'
      }

    let parse_card_type raw_type =
      match raw_type with
      | 0 -> Card_type.Creature
      | 1 -> Card_type.Item_green
      | 2 -> Card_type.Item_red
      | 3 -> Card_type.Item_blue
      | _ -> failwith (Printf.sprintf "Unknown card type: %d" raw_type)

    let parse_card raw_card =
      { Card.number = raw_card.number
      ; id = raw_card.id
      ; type_ = parse_card_type raw_card.type_
      ; cost = raw_card.cost
      ; attack = raw_card.attack
      ; defense = raw_card.defense
      ; abilities = parse_abilities raw_card.abilities
      ; own_health_change = raw_card.own_health_change
      ; opponent_health_change = raw_card.opponent_health_change
      ; draw = raw_card.draw
      }
  end

  let load_round ~count ~self ~opponent ~opponent_hand_length ~cards =
    if count < 30 then
      Round.Draft
        { cards = List.map Card.parse_card cards
        }
    else
      let self_board =
        cards
        |> List.filter (fun card -> card.Card.location == 1)
        |> List.map Card.parse_card
        |> List.map (fun card -> {Board_card.card; can_attack = true})
      in
      let self_hand =
        cards
        |> List.filter (fun card -> card.Card.location == 0)
        |> List.map Card.parse_card
      in
      let opponent_board =
        cards
        |> List.filter (fun card -> card.Card.location == -1)
        |> List.map Card.parse_card
      in
      Round.Battle
        { self =
            { Self.health = self.Player.health
            ; mana = self.Player.mana
            ; deck = self.Player.deck
            ; rune = self.Player.rune
            ; board = self_board
            ; hand = self_hand
            }
        ; opponent =
            { Opponent.health = opponent.Player.health
            ; mana = opponent.Player.mana
            ; deck = opponent.Player.deck
            ; rune = opponent.Player.rune
            ; board = opponent_board
            ; hand_length = opponent_hand_length
            }
        }

  let action_to_string action =
    match action with
    | Action.Pick {position} ->
      Printf.sprintf "PICK %d" position
    | Action.Summon {id} ->
      Printf.sprintf "SUMMON %d" id
    | Action.Attack {attacker_id; target_id} ->
      Printf.sprintf "ATTACK %d %d" attacker_id target_id
    | Action.Pass ->
      "PASS"

  let dump_actions actions =
    match actions with
    | [] ->
      "PASS"
    | _ ->
      actions
      |> List.map action_to_string
      |> String.concat ";"
end

let rec loop count =
  let self = Scanf.sscanf (input_line stdin) "%d %d %d %d" Raw.Player.of_params in
  let opponent = Scanf.sscanf (input_line stdin) "%d %d %d %d" Raw.Player.of_params in
  let opponent_hand_length = int_of_string (input_line stdin) in
  let card_count = int_of_string (input_line stdin) in
  let cards =
    List.init
      card_count
      (fun _ -> Scanf.sscanf (input_line stdin) "%d %d %d %d %d %d %d %s %d %d %d" Raw.Card.of_params)
  in
  let output =
    Raw.load_round ~count ~self ~opponent ~opponent_hand_length ~cards
    |> Strategy.actions
    |> Raw.dump_actions
  in
  print_endline output;
  loop (count + 1)

let main () =
  loop 0
