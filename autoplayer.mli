(**[get_straights hand] is a list of tiles from [hand] that make up
   disjoint combinations of 3 tiles that are in the same suite and are
   consecutive numerically. 
   Example: [get_straights [Stick 1; Stick 2; Stick 3; Disc 4;] =
   [Stick 1; Stick 2; Stick 3]] *)
val get_straights : Deck.tile list -> Deck.tile list

(**[get_discards hand difficulty] is a list of tiles from [hand] that should be 
   discarded based on [difficulty] in order to improve chances of winning. 
   Cards that are part of a triple formation like three of a kind or a straight 
   should not be discarded. *)
val get_discards : Deck.tile list -> State.difficulty -> Deck.tile list 

(**[get_discard_moves s id] is a list of discard commands corresponding
   to the tiles that player [id] should discard in order to improve chances of 
   winning. *)
val get_discard_moves : State.t -> int -> Command.command list

(**[get_play_moves s id] is a card-acquiring command corresponding
   to an action that player [id] should perform in order to best improve chances 
   of winning. It is always Draw, Bump, or Eat. *)
val get_play_moves : State.t -> int -> Command.command