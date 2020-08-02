(** [format_current st id] is the formatted string of the current hand of player
    [id] in state [st]. *)
val format_current : State.t -> int -> string

(** [format_display st id] is the formatted string of the display hand of player
    [id] in state [st]. *)
val format_display : State.t -> int -> string

(** [format_eat lst acc opts] is the formatted string of [lst], the player's 
    possible current eat combinations. *)
val format_eat : (Deck.tile * Deck.tile * Deck.tile) list -> string -> int -> 
  string