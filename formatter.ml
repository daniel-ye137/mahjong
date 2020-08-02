let format_current st id = 
  let hand = State.get_current_hand st id in
  String.concat ", " (List.map Deck.format hand)

let format_display st id = 
  let hand = State.get_display_hand st id in 
  String.concat ", " (List.map Deck.format hand)

let rec format_eat lst acc opts = 
  match lst with 
  | (t1, t2, t3) :: t -> let remaining = List.length t in
    format_eat t 
      (acc ^ "Combination " ^ string_of_int (opts - remaining - 1) ^ " (" ^ 
       Deck.format t1 ^ ", " ^ Deck.format t2 ^ ", " ^ Deck.format t3 ^ "); ") 
      opts
  | [] -> let len = String.length acc in 
    if len = 0 then "" else String.sub acc 0 (len-2)