(** The type [tile] represents the different tiles that make up a deck. *)
type tile = Disc of int | Stick of int | Tens of int |North | East | South | 
            West | Green | Red | White

(** The abstract type of values representing a game's deck. *)
type t

(** The exception [InvalidTile] is raised when an invalid tile is encountered.*)
exception InvalidTile

(** The exception [EmptyDeck] is raised when an empty deck is encountered. *)
exception EmptyDeck

(** [init] returns a randomly ordered deck of tiles. *)
val init : t 

(** [empty] returns the empty deck. *)
val empty : t

(** [remove tile deck] returns [deck] with the tile [tile] removed from it. *)
(* val remove : tile -> t -> t *)

(** [draw deck] returns a tuple containing the first tile of [deck] and the 
    rest of the deck. *)
val draw : t -> (tile * t)

(** [format tile] is the string representation of tile [tile]. *)
val format : tile -> string

(** [off_top deck n] is the first [n] tiles off the top of the deck [deck]. *)
val off_top : t -> int -> t

(** [order tile] is the order value of tile [tile]. *)
val order: tile -> int

(** [print_deck deck] prints [deck] to the console. *)
val print_deck : t -> unit

(** [from_tile_list deck] is [deck] represented as a Deck.t type. *)
val from_tile_list : tile list -> t

(** [shuffle lst] returns the deck [lst] with its elements randomly ordered. *)
val shuffle : tile list -> tile list

(** [get_stick_num init x] is the int list representing the tile list of Stick
    tiles [x]. *)
val get_stick_num : int list -> tile -> int list

(** [get_tens_num init x] is the int list representing the tile list of Tens
    tiles [x]. *)
val get_tens_num : int list -> tile -> int list 

(** [get_disc_num init x] is the int list representing the tile list of Disc
    tiles [x]. *)
val get_disc_num : int list -> tile -> int list

(** [get_num] is the integer value of a suited tile [x]. *)
val get_num : tile -> int