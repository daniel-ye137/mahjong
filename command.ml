(* open Deck *)

type tile_phrase = string list

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

exception Empty

exception Malformed

let parse (str : string) : command = 
  if str = "" then raise Empty else
    let phrase_list = String.split_on_char ' ' str in 
    match phrase_list with 
    | [] -> raise Empty 
    | h :: [] ->
      if h = "draw" then Draw 
      else if h = "eat" then Eat 
      else if h = "bump" then Bump
      else if h = "win" then Win 
      else if h = "help" then Help 
      else if h = "quit" then Quit
      else if h = "start" then Start
      else if h = "pass" then Pass
      else if h = "gang" then Gang
      else raise Malformed 
    | h :: t -> 
      if h = "discard" then (Discard t) 
      else raise Malformed

let tile_from_phrase tile_phrase =
  match tile_phrase with
  | [] -> raise Empty
  | x :: ["Disc"] -> if x = "1" then Deck.Disc 1 else raise Malformed
  | x :: ["Discs"] -> if x = "1" then raise Malformed else 
      Deck.Disc (int_of_string x)
  | x :: ["Stick"] -> if x = "1" then Deck.Stick 1 else raise Malformed
  | x :: ["Sticks"] -> if x = "1" then raise Malformed else 
      Deck.Stick (int_of_string x)
  | x :: ["Ten"] -> if x = "1" then Deck.Tens 1 else raise Malformed
  | x :: ["Tens"] -> if x = "1" then raise Malformed else 
      Deck.Tens (int_of_string x)
  | ["West"] -> Deck.West
  | ["East"] -> Deck.East
  | ["North"] -> Deck.North
  | ["South"] -> Deck.South
  | ["Green"] -> Deck.Green
  | ["White"] -> Deck.White
  | ["Red"] -> Deck.Red
  | _ -> raise Malformed

let phrase_from_tile tile =
  match tile with
  | Deck.Disc 1 -> "1" :: ["Disc"]
  | Deck.Disc x -> (string_of_int x) :: ["Discs"]
  | Deck.Stick 1 -> "1" :: ["Stick"]
  | Deck.Stick x -> (string_of_int x) :: ["Sticks"]
  | Deck.Tens 1 -> "1" :: ["Ten"]
  | Deck.Tens x -> (string_of_int x) :: ["Tens"]
  | Deck.West -> ["West"]
  | Deck.East -> ["East"]
  | Deck.North -> ["North"]
  | Deck.South -> ["South"]
  | Deck.Green -> ["Green"]
  | Deck.White -> ["White"]
  | Deck.Red -> ["Red"]