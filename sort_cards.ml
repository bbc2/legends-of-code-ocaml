let compare_cards card_0 card_1 =
  compare (Legends.Strategy.rate_card card_0) (Legends.Strategy.rate_card card_1)

let parse_card_type type_ =
  match type_ with
  | "creature" -> Legends.Card_type.Creature
  | "itemRed" -> Legends.Card_type.Item_red
  | "itemGreen" -> Legends.Card_type.Item_green
  | "itemBlue" -> Legends.Card_type.Item_blue
  | _ -> failwith (Printf.sprintf "Unknown card type: %s" type_)

let dump_card_type type_ =
  match type_ with
  | Legends.Card_type.Creature -> "creature"
  | Legends.Card_type.Item_red -> "red item"
  | Legends.Card_type.Item_green -> "green item"
  | Legends.Card_type.Item_blue -> "blue item"

let dump_ability symbol value =
  match value with
  | true -> symbol
  | false -> '-'

let dump_abilities abilities =
  let open Legends.Abilities in
  let chars = 
    [ dump_ability 'B' abilities.breakthrough
    ; dump_ability 'C' abilities.charge
    ; dump_ability 'G' abilities.guard
    ; dump_ability 'D' abilities.drain
    ; dump_ability 'L' abilities.lethal
    ; dump_ability 'W' abilities.ward
    ]
  in
  chars
  |> List.to_seq
  |> String.of_seq

let parse_card line =
  let values =
    line
    |> String.split_on_char ';'
    |> List.map String.trim
  in
  match values with
  | [ number
    ; _name
    ; type_
    ; cost
    ; attack
    ; defense
    ; abilities
    ; own_health_change
    ; opponent_health_change
    ; draw
    ; _summary
    ] ->
    { Legends.Card.number = int_of_string number
    ; id = 0
    ; type_ = parse_card_type type_
    ; cost = int_of_string cost
    ; attack = int_of_string attack
    ; defense = int_of_string defense
    ; abilities = Legends.Raw.Card.parse_abilities abilities
    ; own_health_change = int_of_string own_health_change
    ; opponent_health_change = int_of_string opponent_health_change
    ; draw = int_of_string draw
    }
  | _ ->
    failwith "Wrong number of columns"

let dump_card card =
  let open Legends.Card in
  Printf.sprintf
    "%d,%s,%d,%d,%d,%s,%d,%d,%d"
    card.number
    (dump_card_type card.type_)
    card.attack
    card.defense
    card.cost
    (dump_abilities card.abilities)
    card.own_health_change
    card.opponent_health_change
    card.draw

let () =
  let cards_txt = [%blob "cards.txt"] in
  let lines = String.split_on_char '\n' cards_txt in
  let sorted_lines =
    lines
    |> List.filter (fun line -> line <> "")
    |> List.map parse_card
    |> List.sort compare_cards
    |> List.map dump_card
  in
  let output = String.concat "\n" sorted_lines in
  Printf.printf "%s\n" output
