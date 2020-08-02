(* The exception [UnknownTile] is raised when an unknown tile is encountered. *)
exception UnknownTile

(* The exception [UnknownDifficulty] is raised when an unknown difficulty is 
   encountered. *)
exception UnknownDifficulty

(** The type reprsenting the difficulty of autoplayers. *)
type difficulty = Easy | Hard

(** The type of values representing the game state. *)
type t = {
  (** A player id, tile list association list that represents the current hands 
      of all players. *)
  current : (int * Deck.tile list) list;
  (** The id of the player whose turn it is. *)
  curr_id : int;
  (** A player id, tile list association list that represents the displayed 
      hands from eating and bumping of all players. *)
  display: (int * Deck.tile list) list;
  (** The remaining tiles in the deck. *)
  remaining : Deck.t;
  (** A list of all discarded tiles. *)
  discarded : Deck.tile list;
  (** The difficulty of the autoplayers. *)
  difficulty : difficulty
}

(** The type representing the result of an attempted action. *)
type result = Legal of t | Illegal

(** [parse_difficulty str] is the difficulty corresponding to [str]. *)
val parse_difficulty : string -> difficulty

(** [get_difficulty state] is the difficulty of [state]. *)
val get_difficulty : t -> difficulty

(** [init_state difficulty] is the initial state of the game. In this state, 
    everyone has 13 tiles, it is player 0's turn, there are no discarded tiles, 
    and the autoplayer difficulty is [difficulty]. *)
val init_state :  difficulty -> t

(** [get_curr_id st] is the id of the player whose turn it is in state [st]. *)
val get_curr_id : t -> int

(** [set_id st id] is the state [st] with its curr_id set to [id]*)
val set_id : t -> int -> t

(** [compare_tile x y] returns 0 if x is equal to y, a negative integer if x is 
    less than y, and a positive integer if x is greater than y. *)
val compare_tile : Deck.tile -> Deck.tile -> int

(** [get_current_hand st player_id] is the current hand of player [player_id] in
    state [st]. *)
val get_current_hand : t -> int -> Deck.tile list

(** [get_display_hand st player_id] is the displayed hand of player [player_id] 
    in state [st]. *)
val get_display_hand : t -> int -> Deck.tile list

(** [remaining st] is the remaining tiles in the deck of state [st]. *)
val remaining : t -> Deck.t

(** [discarded st] is the list of discarded tiles of state [st]. *)
val discarded : t -> Deck.tile list

(** [draw st player_id] is the result of player whose id is [player_id] in state
    [st] drawing a tile. *)
val draw : t -> int -> result

(** [last_discarded st] is the tile that was most recently discarded in state 
    [st]. *)
val last_discarded : t -> Deck.tile

(**[check_eat hand discard] is a list of tuples of 3 consecutive numeric tiles
   of the same suite made up from [discard] and elements of [hand]. If no such
   tuples exist, it is None. 
   Example: [check_eat [Stick 1; Stick 2; Stick 4; Stick 5] (Stick 3) = 
   Some [(Stick 1, Stick 2, Stick 3); (Stick 2, Stick 3, Stick 4); 
   (Stick 3, Stick 4, Stick 5)] ]*)
val check_eat : Deck.tile list -> Deck.tile -> 
  (Deck.tile * Deck.tile * Deck.tile) list option

(** [eat player_id st tiles] is the result of player whose id is [player_id] in 
    state [st] eating the most recently discarded tile. If the player can eat, 
    then the result is [Legal st'] where [st'] is the new game state with the
    player's display and current hands as well as the game's discarded field 
    updated. Otherwise, the result is [Illegal]. *)
val eat : int -> t -> (Deck.tile * Deck.tile * Deck.tile) -> result

(** [index lst idx] is the element at position [idx] in list [lst]. *)
val index : (Deck.tile * Deck.tile * Deck.tile) list -> int -> 
  (Deck.tile * Deck.tile * Deck.tile)

(** [check_bump hand discard] is a tuple representing the 3 identical tiles made 
    up from [discard] and elements of [hand] that can be bumped. If no such 
    tuples exist, it is None. *)
val check_bump : Deck.tile list -> Deck.tile -> 
  (Deck.tile * Deck.tile * Deck.tile list) option

(** [bump player_id st] is the result of player whose id is [player_id] in state
    [st] bumping the most recently discarded tile. If the player can bump, then 
    the result is [Legal st'] where [st'] is the new game state with the 
    player's display and current hands as well as the game's discarded field 
    updated. Otherwise, the result is [Illegal]. *)
val bump : int -> t -> result

(** [discard tile st] is the result of discarding [tile] from the current 
    player's hand in state [st]. If the tile exists in the player's hand and is 
    removed, the result is [Legal st'] where [st'] is the new game state with 
    the player's current hand and the game's discarded field updated. Otherwise,
    the result is [Illegal]. *)
val discard : Deck.tile -> t -> result

(** [check_pair hand acc] returns true if [hand] is a valid winning hand with 
    one pair and 4 sets of triples or straights. For every possible pair of 
    tiles in [hand], return true if the tiles [hand] with this pair removed
    only contains valid triples or straights, otherwise false if the tiles 
    do not form a winning hand.*)
val check_pair : Deck.tile list -> Deck.tile list -> bool 

(** [win_hand st id flag] returns the sorted list of the current hand 
    and displayed hand of player [id], and the last discarded card if 
    [flag] is true. *)
val win_hand : t -> int -> bool -> Deck.tile list

(** [win_helper hand] returns true if [hand] only contains valid triple or 
    straight tiles, and false otherwise. *)
val win_helper : Deck.tile list -> bool

(** [remove_two_tiles tile1 tile2 tile_list acc flag] returns the [tile_list]
    with [tile1] and [tile2] is removed if [flag] is false. *)
val remove_two_tiles : Deck.tile -> Deck.tile -> Deck.tile list -> 
  Deck.tile list -> bool -> Deck.tile list

(** [check_gang hand discard] is the Deck.tile list option representing 4 
    identical tiles made up from [discard] and elements of [hand] that can be 
    ganged. If no such tuples exist, it is None. *)
val check_gang : Deck.tile list -> Deck.tile -> (Deck.tile list) option

(** [gang player_id st] is the reuslt of player [player_id] in state [st] 
    ganging the most recently discarded tile. If the player can gang, then the 
    result is [Legal st'] where [st'] is the new game state with the player's 
    display and current hands as well as the game's discarded field updated. 
    Otherwise, the result is [Illegal]. *)
val gang : int -> t -> result

(** [gang_hand player_id st] is the result of player whose id is [player_id] in
    state [st] ganging a set of four identical tiles in their hand. If the 
    player can gang, then the result is [Legal st'] where [st'] is the new game
    state with the player's display and current hands as well as the game's 
    discarded field updated. Otherwise, the result is [Illegal]. *)
val gang_hand : int -> t -> result

(** [new_draw st] is the string representing the next tile of the remaining deck
    in game [st]. *)
val next_draw : t -> string