open Deck

type item_name = string

exception UnknownTile

exception UnknownDifficulty

type difficulty = Easy | Hard

type t = {
  current : (int * Deck.tile list) list;
  curr_id : int;
  display: (int * Deck.tile list) list;
  remaining : Deck.t;
  discarded : Deck.tile list;
  difficulty : difficulty
}

type result = Legal of t | Illegal

let parse_difficulty str = 
  match str with 
  | "easy" -> Easy 
  | "hard" -> Hard
  | _ -> raise UnknownDifficulty

let get_difficulty state = 
  state.difficulty

(** [init_hand acc deck counter] is the initialized hand of a single player 
    given deck [deck] at the start of a new game. *)
let rec init_hand acc deck counter =
  if counter = 0 then acc else let draw = Deck.draw deck in
    match acc with 
    |(x, []) -> 
      init_hand (x, fst draw :: []) (snd draw) (counter -1)
    |(x, y) ->
      init_hand (x, (fst draw) :: (snd acc)) (snd draw) (counter -1)

(** [init_hands deck] is the initialized hands of all players given deck [deck] 
    at the start of a new game. *)
let init_hands deck = 
  (init_hand (0, []) deck 13) :: (init_hand (1, []) (Deck.off_top deck 13) 13)::
  (init_hand (2, []) (Deck.off_top deck 26) 13) :: 
  (init_hand (3, []) (Deck.off_top deck 39) 13) :: []

let init_state level =
  let deck = Deck.init in 
  {
    current = init_hands deck;
    curr_id = 0;
    display = [(0, []); (1, []); (2, []); (3, [])];
    remaining = (Deck.off_top deck 52);
    discarded = [];
    difficulty = level
  }

let get_curr_id st = st.curr_id

let set_id st id = {st with curr_id = id}

let compare_tile x y =
  if Deck.order x > Deck.order y then 1
  else if Deck.order x < Deck.order y then -1
  else 0

let rec get_current_hand st player_id = 
  match st.current with
  | (id, lst) :: t -> if id = player_id then List.sort compare_tile lst else 
      get_current_hand {st with current = t} player_id 
  | [] -> failwith ("invalid player")

let rec get_display_hand st player_id = 
  match st.display with
  | (id, lst) :: t -> if id = player_id then List.sort compare_tile lst else 
      get_display_hand {st with display = t} player_id
  | [] -> failwith ("invalid player")

let remaining st =
  st.remaining

let discarded st = 
  st.discarded

(** [add_tile curr player_id tile acc] returns the player id, tile list
    association list [curr] where the player whose id is [player_id] has had 
    [tile] added to their tile list.*)
let rec add_tile curr player_id tile acc = 
  match curr with 
  | (id, lst) :: t -> if id = player_id then add_tile t player_id tile 
        ((id, tile::lst)::acc) else add_tile t player_id tile ((id,lst)::acc)
  | [] -> acc

(** [remove_tile_helper tile tile_list acc flag] returns the tile list 
    [tile_list] with tile [tile] removed. *)
let rec remove_tile_helper tile tile_list acc flag = 
  match tile_list with
  | h :: t -> if h = tile && flag = false then 
      remove_tile_helper tile t acc true else 
      remove_tile_helper tile t (h :: acc) flag
  | [] -> List.rev acc

(** [remove_tile curr player_id tile acc] returns the player id, tile 
    association list [curr] where the player whose id is [player_id] has had 
    [tile] removed from their tile list.*)
let rec remove_tile curr player_id tile acc = 
  (* let () = print_string (string_of_int player_id) in *)
  match curr with 
  | (id, lst) :: t -> if id = player_id then 
      remove_tile t player_id tile 
        ((id, (remove_tile_helper tile lst [] false) ):: acc) else
      remove_tile t player_id tile ((id,lst) :: acc)
  | [] -> acc

let draw st player_id =
  let drawn = Deck.draw st.remaining in 
  let tile = fst drawn in 
  let new_deck = snd drawn in
  Legal {st with current = add_tile st.current player_id tile [];
                 remaining = new_deck;
        }

let last_discarded st = 
  match st.discarded with 
  | h :: t -> h 
  | [] -> raise UnknownTile

(** [check_eat_helper int_list discard] is a list of tuples of 3 consecutive
    numbers made up of the number from [discard] and elements of [int_list]. If
    no such tuples exist, it is None. *)
let check_eat_helper int_list discard =
  let card_num = Deck.get_num discard in 
  let eat_list =
    if List.mem (card_num-1) int_list &&  List.mem (card_num-2) int_list then
      [(card_num - 2, card_num - 1, card_num)] else [] in
  let eat_list' = 
    if List.mem (card_num-1) int_list &&  List.mem (card_num+1) int_list then
      [(card_num - 1, card_num, card_num + 1)] @ eat_list else eat_list in
  let eat_list'' = 
    if List.mem (card_num+1) int_list &&  List.mem (card_num+2) int_list then
      [(card_num, card_num+1, card_num+2)] @ eat_list' else eat_list' in
  if List.length eat_list'' = 0 then None else
    Some (List.rev eat_list'')

let check_eat hand discard = 
  match discard with 
  | Stick x -> begin 
      let stick_list = List.sort compare 
          (List.fold_left Deck.get_stick_num [] hand) in 
      let nums = check_eat_helper stick_list discard in 
      match nums with 
      | Some lst -> Some (List.map (fun (n1, n2, n3) -> 
          (Stick n1, Stick n2, Stick n3)) lst)
      | None -> None
    end
  | Tens x -> begin
      let tens_list = List.fold_left Deck.get_tens_num [] hand in
      let nums = check_eat_helper tens_list discard in
      match nums with 
      | Some lst -> Some (List.map (fun (n1, n2, n3) -> 
          (Tens n1, Tens n2, Tens n3)) lst)
      | None -> None
    end
  | Disc x -> begin
      let disc_list = List.fold_left Deck.get_disc_num [] hand in
      let nums = check_eat_helper disc_list discard in 
      match nums with 
      | Some lst -> Some (List.map (fun (n1, n2, n3) -> 
          (Disc n1, Disc n2, Disc n3)) lst)
      | None -> None
    end
  | _ -> None

let eat player_id st tiles = 
  let discarded_tile = last_discarded st in
  let tiles = match tiles with 
    | (t1, t2, t3) -> if t1 = discarded_tile then (t2, t3) 
      else if t2 = discarded_tile then (t1, t3)
      else (t1, t2) in 
  match tiles with 
  | (tile1, tile2) -> 
    Legal {st with current = (remove_tile 
                                (remove_tile st.current player_id tile1 []) 
                                player_id tile2 []);
                   display = (add_tile 
                                (add_tile 
                                   (add_tile st.display player_id tile1 []) 
                                   player_id tile2 []) 
                                player_id discarded_tile []);
                   discarded = remove_tile_helper discarded_tile 
                       st.discarded [] false}
  | _ -> failwith "You should never reach this path from eat."

let rec index lst idx = 
  match lst with 
  | h :: t -> if idx = 0 then h else index t (idx - 1)
  | _ -> failwith "index out of bounds"

(** [list_to_tuple lst] is the tuple representing the list [lst]. *)
let list_to_tuple lst = 
  match lst with
  | a :: b :: c -> (a, b, c)
  | _-> failwith "incorrect usage"

let check_bump hand discard = 
  let filtered_hand = List.filter (fun x -> x = discard) hand in
  if List.length filtered_hand = 2 then Some (list_to_tuple (
      discard::filtered_hand)) else None

let bump player_id st = 
  let curr_hand = get_current_hand st player_id in 
  let discarded_tile = last_discarded st in
  let can_bump = check_bump curr_hand discarded_tile in 
  match can_bump with 
  | Some (t1, t2, t3) -> 
    Legal {st with curr_id = player_id;
                   current = (remove_tile 
                                (remove_tile st.current player_id t1 []) 
                                player_id t1 []); 
                   display = (add_tile 
                                (add_tile 
                                   (add_tile st.display player_id t1 []) 
                                   player_id t1 []) player_id t1 []); 
                   discarded = remove_tile_helper t1 st.discarded [] false}
  | None -> Illegal

let discard tile st = 
  let curr = get_current_hand st st.curr_id in
  let id = st.curr_id in
  if List.mem tile curr then 
    Legal {
      st with curr_id = ((st.curr_id + 1) mod 4);
              current = remove_tile st.current id tile [];
              discarded = tile :: st.discarded
    }
  else Illegal

let remove_two_tiles tile1 tile2 tile_list acc flag  = 
  remove_tile_helper tile1
    (remove_tile_helper tile2 tile_list acc flag) [] flag

let rec win_helper hand =
  match hand with
  | x :: y :: z :: t when x = y && y = z -> win_helper t
  | Disc a :: t when List.mem (Disc (a - 1)) t && List.mem (Disc (a - 2)) t
    -> remove_two_tiles (Disc (a - 1)) (Disc (a - 2)) t []  false
       |> win_helper
  | Stick a :: t when List.mem (Stick (a - 1)) t && List.mem (Stick (a - 2)) t
    -> remove_two_tiles (Stick (a - 1)) (Stick (a - 2)) t []  false
       |> win_helper
  | Tens a :: t when List.mem (Tens (a - 1)) t && List.mem (Tens (a - 2)) t
    -> remove_two_tiles (Tens (a - 1)) (Tens (a - 2)) t []  false
       |> win_helper
  | [] -> true
  | _ -> false

(** [win hand pair] returns true if the tiles in [hand] with the tiles from 
    [pair] are removed form a valid winning hand, otherwise fasle*)
let win (hand : tile list) (pair : (tile * tile)) = 
  let clean = match pair with
    | (a, b) -> remove_two_tiles a b hand [] false
  in if List.length clean = 12 then List.rev clean
                                    |> win_helper 
  else false

let rec check_pair (hand : tile list) (acc : tile list) = 
  match acc with
  | x :: t -> if List.mem x t then
      (if win hand (x, x) then true else check_pair hand t) 
    else check_pair hand t
  | _ -> false

let win_hand st id flag = 
  let all_tiles =  (get_current_hand st id)@(get_display_hand st id) 
                   |> List.sort compare_tile in 
  if flag then (last_discarded st) :: all_tiles
               |> List.sort compare_tile
  else all_tiles

let check_gang hand discard = 
  let filtered_hand = List.filter (fun x -> x = discard) hand in
  if List.length filtered_hand = 3 then Some (discard :: filtered_hand)
  else None

let gang player_id st = 
  let curr_hand = get_current_hand st player_id in 
  let discarded_tile = last_discarded st in
  let can_gang = check_gang curr_hand discarded_tile in 
  match can_gang with 
  | Some [t1; t2; t3; t4] -> let draw = Deck.draw st.remaining in 
    begin
      match draw with
      | (tile, deck') ->
        Legal {st with curr_id = player_id;
                       current = (add_tile 
                                    (remove_tile 
                                       (remove_tile 
                                          (remove_tile st.current player_id t1 
                                             []) 
                                          player_id t1 []) 
                                       player_id t1 [])
                                    player_id tile []);
                       display = (add_tile 
                                    (add_tile 
                                       (add_tile 
                                          (add_tile st.display player_id t1 []) 
                                          player_id t1 []) 
                                       player_id t1 []) 
                                    player_id t1 []);
                       remaining = deck';
                       discarded = remove_tile_helper t1 st.discarded [] false}
    end
  | _ -> Illegal

(** [gang_hand_helper hand] returns the list of four identical tiles if they 
    exist in [hand]. If no such set exists, returns the empty list. *)
let rec gang_hand_helper hand = 
  match hand with 
  | [] -> []
  | h :: t -> let filtered_hand = List.filter (fun x -> x = h) hand in
    let nums = List.length filtered_hand in 
    if nums = 4 then [h; h; h; h]
    else gang_hand_helper t

let gang_hand player_id st = 
  let curr_hand = get_current_hand st player_id in 
  let result = gang_hand_helper curr_hand in 
  match result with 
  | [t1; t2; t3; t4] -> let draw = Deck.draw st.remaining in 
    begin
      match draw with 
      | (tile, deck') ->
        Legal {st with current = add_tile 
                           (remove_tile 
                              (remove_tile 
                                 (remove_tile 
                                    (remove_tile st.current player_id t1 []) 
                                    player_id t1 []) 
                                 player_id t1 []) 
                              player_id t1 [])
                           player_id tile [];
                       display = (add_tile 
                                    (add_tile 
                                       (add_tile 
                                          (add_tile st.display player_id t1 []) 
                                          player_id t1 []) 
                                       player_id t1 []) 
                                    player_id t1 []);
                       remaining = deck';
              }
    end
  | _ -> Illegal

let next_draw st = 
  match Deck.draw (remaining st) with 
  | (h, t) -> Deck.format h 
  | _ -> raise UnknownTile