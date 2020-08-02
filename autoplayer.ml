open Deck

(**[get_triples_helper h a] is an association list of (Deck.tile, int) that
   maps a tile to how many times it appears in [hand] *)
let rec get_triples_helper hand acc = 
  match hand with
  | h :: t -> if List.mem (h, 3) acc then 
      get_triples_helper t ((h, 1) :: acc) else
    if List.mem (h, 2) acc then 
      get_triples_helper t 
        ((h, 3) :: (List.filter (fun (x, y) -> (x, y) <> (h,2)) acc)) else
    if List.mem (h, 1) acc then 
      get_triples_helper t 
        ((h, 2) :: (List.filter (fun (x, y) -> (x, y) <> (h,1)) acc)) else
      get_triples_helper t 
        ((h, 1) :: acc)
  | [] -> acc 

(**[remove_single t f a h] is [hand] with one instance of [tile] removed
   from it. *)
let rec remove_single tile flag acc hand = 
  match hand with
  | h :: t -> if h = tile && flag = false then 
      remove_single tile true acc t else 
      remove_single tile flag (h :: acc) t
  | [] -> List.sort (State.compare_tile) acc

(**[difference lst1 lst2] is [lst1] but for every instance of a tile in
   lst2, that same tile is removed once from lst1.*)
let rec difference lst1 lst2 = 
  match lst2 with 
  | h :: t -> difference (remove_single h false [] lst1) t
  | [] -> lst1

(**[get_straight_helper hand acc] is a list of tiles from [hand] that make up
   disjoint combinations of 3 tiles that are in the same suite and are
   consecutive numerically. 
   Example: [get_straights [Stick 1; Stick 2; Stick 3; Disc 4;] =
   [Stick 1; Stick 2; Stick 3]] *)
let rec get_straight_helper hand acc = 
  begin
    match hand with 
    | [] -> acc
    | h :: t -> 
      begin
        match h with 
        | Stick x -> 
          if List.mem (Stick (x + 1)) hand && List.mem (Stick(x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Stick x) false [] 
              |> remove_single (Stick (x + 1)) false [] 
              |> remove_single (Stick (x + 2)) false [] in 
            let new_acc = (Stick x)::(Stick (x + 1))::(Stick (x + 2))::acc in
            get_straight_helper new_hand new_acc 
          else get_straight_helper t acc
        | Disc x -> 
          if List.mem (Disc (x + 1)) hand && List.mem (Disc(x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Disc x) false [] 
              |> remove_single (Disc (x + 1)) false [] 
              |> remove_single (Disc (x + 2)) false [] in 
            let new_acc = (Disc x) :: (Disc (x + 1)) :: (Disc (x + 2)) :: acc in
            get_straight_helper new_hand new_acc 
          else get_straight_helper t acc
        | Tens x -> 
          if List.mem (Tens (x + 1)) hand && List.mem (Tens(x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Tens x) false [] 
              |> remove_single (Tens (x + 1)) false [] 
              |> remove_single (Tens (x + 2)) false [] in 
            let new_acc = (Tens x) :: (Tens (x + 1)) :: (Tens (x + 2)) :: acc in
            get_straight_helper new_hand new_acc 
          else get_straight_helper t acc
        | _ -> failwith "bad type"
      end
  end

(**[get_adjacent_helper hand acc] is a list of tiles from [hand] that make up
   disjoint combinations of 2 tiles that are one tile away from making 
   a formation of 3 numerically consecutive tiles of the same suite.
   Example: [get_straights [Stick 1; Stick 3; Stick 7; Disc 3; Disc 4] =
   [Stick 1; Stick 3; Disc 3; Disc 4]] *)
let rec get_adjacent_helper hand acc = 
  begin
    match hand with 
    | [] -> acc
    | h :: t -> 
      begin
        match h with 
        | Stick x -> 
          if List.mem (Stick (x + 1)) hand then
            let new_hand = 
              hand |> remove_single (Stick x) false [] 
              |> remove_single (Stick (x + 1)) false [] in 
            let new_acc = (Stick x) :: (Stick (x + 1)) :: acc in
            get_adjacent_helper new_hand new_acc 
          else if List.mem (Stick (x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Stick x) false [] 
              |> remove_single (Stick (x + 2)) false [] in 
            let new_acc = (Stick x) :: (Stick (x + 2)) :: acc in
            get_adjacent_helper new_hand new_acc else
            get_adjacent_helper t acc
        | Disc x -> 
          if List.mem (Disc (x + 1)) hand then
            let new_hand = 
              hand |> remove_single (Disc x) false [] 
              |> remove_single (Disc (x + 1)) false [] in 
            let new_acc = (Disc x) :: (Disc (x + 1)) :: acc in
            get_adjacent_helper new_hand new_acc 
          else if List.mem (Disc (x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Disc x) false [] 
              |> remove_single (Disc (x + 2)) false [] in 
            let new_acc = (Disc x) :: (Disc (x + 2)) :: acc in
            get_adjacent_helper new_hand new_acc  else 
            get_adjacent_helper t acc
        | Tens x -> 
          if List.mem (Tens (x + 1)) hand then
            let new_hand = 
              hand |> remove_single (Tens x) false [] 
              |> remove_single (Tens (x + 1)) false [] in 
            let new_acc = (Tens x) :: (Tens (x + 1)) :: acc in
            get_adjacent_helper new_hand new_acc 
          else if List.mem (Tens (x + 2)) hand then
            let new_hand = 
              hand |> remove_single (Tens x) false [] 
              |> remove_single (Tens (x + 2)) false [] in 
            let new_acc = (Tens x) :: (Tens (x + 2)) :: acc in
            get_adjacent_helper new_hand new_acc 
          else get_adjacent_helper t acc 
        | _ -> failwith "bad type"
      end
  end

(**[add_tuples lst acc] is a list of tiles formed by taking every element 
   (t, i) of [lst] and adding [i] instances of tile [t] to [acc]*)
let rec add_tuples lst acc = 
  match lst with 
  | (x, 1) :: t -> add_tuples t (x :: acc)
  | (x, 2) :: t -> add_tuples t (x :: x :: acc)
  | (x, 3) :: t -> add_tuples t (x :: x :: x :: acc)
  | _ -> acc

let get_straights hand = 
  let nums = List.filter (fun x -> Deck.order x < 28) hand in 
  let sorted_num = List.sort State.compare_tile nums in
  List.sort State.compare_tile (get_straight_helper sorted_num [])

let get_discards hand difficulty = 
  let three_of = add_tuples (List.filter (fun (x,y) -> y > 2) 
                               (get_triples_helper hand [])) [] in 
  let lone_or_pairs = difference hand three_of in 
  let straights = get_straights lone_or_pairs in
  let discards1 = difference lone_or_pairs straights in 
  let discards2 = add_tuples (List.filter (fun (x,y) -> y < 2) 
                                (get_triples_helper discards1 [])) [] in
  let two_consec = get_adjacent_helper 
      (List.filter (fun x -> Deck.order x < 28) discards2) [] in
  let discards3 = difference discards2 two_consec in
  match difficulty with 
  | State.Easy -> begin
      if List.length discards1 = 0 then Deck.shuffle hand 
      else Deck.shuffle discards1
    end
  | State.Hard -> begin
      if List.length discards1 = 0 then Deck.shuffle hand 
      else if List.length discards2 = 0 then Deck.shuffle discards1 
      else if List.length discards3 = 0 then Deck.shuffle discards2 
      else Deck.shuffle discards3
    end

let get_discard_moves state id = 
  let hand = State.get_current_hand state id in 
  let disc_tiles = get_discards hand (State.get_difficulty state) in 
  let discard_moves = 
    List.map (fun x -> Command.Discard (Command.phrase_from_tile x)) disc_tiles
  in if State.check_pair (State.win_hand state id false) 
      (State.win_hand state id false) then [Command.Win] 
  else discard_moves

let get_play_moves state id =
  begin
    match State.discarded state with 
    | h ::t -> let discard = h in 
      let hand = State.get_current_hand state id in
      let can_win = State.check_pair (State.win_hand state id true) 
          (State.win_hand state id true)in 
      if can_win then Command.Win else 
      if id = State.get_curr_id state then 
        let eat = State.check_eat hand discard in 
        let bump = State.check_bump hand discard in 
        begin
          match eat, bump with   
          | None, None -> Command.Draw
          | None, Some x -> Command.Bump
          | Some x, None ->  Command.Eat
          | Some x, Some y -> Command.Bump
        end
      else 
        let bump = State.check_bump hand discard in 
        begin
          match bump with   
          | None -> Command.Draw
          | Some x -> Command.Bump
        end
    | _ -> Command.Draw
  end
