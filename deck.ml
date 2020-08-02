type tile = Disc of int | Stick of int | Tens of int | North | East | South | 
            West | Green | Red | White

type t = tile list

exception InvalidTile

exception EmptyDeck

(** [num_helper typ init lst] returns a tile list containing all numbered tiles 
    of a suit corresponding to [typ]. *)
let rec num_helper typ init lst = 
  begin
    match init with
    | 0 -> lst
    | _ ->  
      begin 
        match typ with 
        | "s" -> num_helper typ (init - 1) ((Stick init)::lst)
        | "d" -> num_helper typ (init - 1) ((Disc init)::lst)
        | "t" -> num_helper typ (init - 1) ((Tens init)::lst)
        | _ -> failwith "bad typing"
      end
  end

let shuffle lst = 
  Random.self_init ();
  let nd = List.map (fun c -> (Random.int 10000, c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond

(** [quad lst] returns a list containing 4 instances of [lst]. *)
let quad lst = 
  lst |> List.append lst |> List.append lst |> List.append lst

let init = 
  let nums_list = [] |> num_helper "s" 9 
                  |> num_helper "d" 9 |> num_helper "t" 9 in 
  let uniq_list =
    East :: West :: North :: South :: Red :: Green :: White :: nums_list in
  uniq_list |> quad |> shuffle

let empty = []

(* let remove tile deck = 
   List.filter (fun x -> x <> tile) deck *)

let draw deck = 
  match deck with 
  | [] -> raise EmptyDeck
  | h :: t -> (h, t)

let format tile = 
  match tile with
  | Tens x -> if x = 1 then (string_of_int x) ^ " Ten" 
    else (string_of_int x) ^ " Tens"
  | Disc x -> if x = 1 then (string_of_int x) ^ " Disc" 
    else (string_of_int x) ^ " Discs"
  | Stick x -> if x = 1 then (string_of_int x) ^ " Stick" 
    else (string_of_int x) ^ " Sticks"
  | East -> "East"
  | West -> "West"
  | North -> "North"
  | South -> "South"
  | Red -> "Red"
  | Green -> "Green"
  | White -> "White"

let rec off_top deck n =
  if n = 0 then deck else 
    match deck with
    | h :: t -> off_top t (n-1)
    | [] -> raise InvalidTile

let order tile =
  match tile with
  | Disc x -> x
  | Stick x -> 9+x
  | Tens x -> 18+x
  | North -> 28
  | East -> 29
  | South -> 30
  | West -> 31
  | Green -> 32
  | Red -> 33
  | White -> 34

let print_deck deck = 
  let () = Printf.printf "%s %d \n" "deck length is" (List.length deck) in
  List.iter (fun x -> Printf.printf "%s " (format x)) deck

let from_tile_list lst = lst

let get_stick_num init x = 
  match x with 
  | Stick num -> num :: init
  | _ -> init

let get_tens_num init x = 
  match x with 
  | Tens num -> num :: init
  | _ -> init

let get_disc_num init x = 
  match x with 
  | Disc num -> num :: init
  | _ -> init

let get_num x = 
  match x with 
  | Stick num -> num
  | Disc num -> num
  | Tens num -> num
  | _ -> failwith "broken"