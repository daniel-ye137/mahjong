(** The type [tile_phrase] represents the tile phrase that can be part of a 
    player command. *)
type tile_phrase = string list

(** The type [command] represents a player command that is broken down into a 
    verb and possibly a tile phrase. *)
type command = 
  | Draw 
  | Eat 
  | Bump 
  | Discard of tile_phrase
  | Win
  | Help
  | Quit 
  | Start
  | Pass
  | Gang

(** The exception [Empty] is raised when an empty command is parsed. *)
exception Empty

(** The exception [Malformed] is raised when a malformed command is parsed. *)
exception Malformed 

(** [parse str] parses a player's input into a [command].*)
val parse : string -> command

(** [tile_from_phrase tile_phrase] parses [tile_phrase] into a tile. *)
val tile_from_phrase : string list -> Deck.tile

(** [phrase_from_tile tile] parses [tile] into a string list representing the 
    tile. *)
val phrase_from_tile : Deck.tile -> string list
