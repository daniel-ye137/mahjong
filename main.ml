(* The structure and implementation of our commands and main modules were 
   inspired by CS 3110 A2 and A3. *)

let help = "This is a turn-based game where you draw and discard tiles to try" ^
           " and make 4 sets of triples (3-in-a-row or 3-of-a-kind) plus 1 " ^
           "set of a double (2-of-a-kind).\n

Game Commands: \n
draw           Draw the next tile in the deck.\n
eat            Eat the tile discarded by the player immediately before you to" ^
           " create a 3-in-a-row. This takes the place of your drawing a tile" ^
           " at the beginning of your turn.\n
bump           Bump the tile discarded by any player to create a 3-of-a-kind." ^ 
           " This takes the place of your drawing a tile at the beginning of " ^ 
           "your turn. Bumping takes precedence over eating.\n
pass           Pass the option to bump or eat a tile.\n
discard TILE   Discard TILE from your hand.\n
win            Win the game when you have 4 sets of triples and 1 set of a " ^
           "double.\n
start          Start the game.\n
quit           Quit the game.\n\n"

(** [state_from_result rs] is the state from a result. *)
let state_from_result rs =
  match rs with
  | State.Legal st -> st
  | State.Illegal -> failwith "improper use of state_from_result"

(** [print_hands st id pid] is the text printed to the terminal that represents
    player 0's current and display hands or autoplayer [id]'s display hand. *)
let print_hands st id pid = 
  if id = 0 then begin
    print_endline "Your hand is now"; 
    print_endline (Formatter.format_current st id ^ "\n");
    print_endline "Your displayed hand is";
    print_endline ((Formatter.format_display st id) ^ "\n");
  end
  else begin
    print_endline "Their displayed hand is";
    print_endline ((Formatter.format_display st id) ^ "\n")
  end

(** [play_turn st] is the state [st] after having a player-specified card
    acquiring action performed on it. [st]. *)
let rec play_turn st = 
  let id = State.get_curr_id st in 
  let player_id = string_of_int id in
  let plays1 = Autoplayer.get_play_moves st 1 in
  let plays2 = Autoplayer.get_play_moves st 2 in 
  let plays3 = Autoplayer.get_play_moves st 3 in
  if plays1 = Command.Win then play_command plays1 st "1" 1 else 
  if plays2 = Command.Win then play_command plays2 st "2" 2 else 
  if plays3 = Command.Win then play_command plays3 st "3" 3 else 
  if plays1 = Command.Bump then play_command plays1 st "1" 1 else 
  if plays2 = Command.Bump then play_command plays2 st "2" 2 else 
  if plays3 = Command.Bump then play_command plays3 st "3" 3 else 
  if (id = 0) then
    begin
      print_hands st id player_id;
      print_endline ("What would you like to do Player " ^ player_id ^ "?");
      print_string "> ";
      match read_line () with 
      | exception End_of_file -> () 
      | command -> 
        try let action = Command.parse command in
          play_command action st player_id id
        with 
        | Command.Empty -> ANSITerminal.(print_string [red]
                                           "\nERROR!\n");
          print_endline ("You typed an empty action. " ^
                         "Please enter a new non-empty draw action.");
          play_turn st
        | Command.Malformed -> ANSITerminal.(print_string [red]
                                               "\nERROR!\n");
          print_endline ("You typed an invalid action. " ^
                         "Please enter a valid draw action.");
          play_turn st 
    end
  else
    begin
      ignore (Unix.select [] [] [] 1.0);
      let plays1 = Autoplayer.get_play_moves st 1 in
      let plays2 = Autoplayer.get_play_moves st 2 in 
      let plays3 = Autoplayer.get_play_moves st 3 in
      if plays1 = Command.Bump || plays1 = Command.Win then play_command 
          plays1 st "1" 1 else 
      if plays2 = Command.Bump || plays2 = Command.Win then play_command 
          plays2 st "2" 2 else 
      if plays3 = Command.Bump || plays3 = Command.Win then play_command 
          plays3 st "3" 3 else 
        let play = Autoplayer.get_play_moves st id in 
        play_command play st player_id id
    end

(** [play_command action st pid id] is the state [st] after [action] has
    been applied to it by player [id]. *)
and play_command (action : Command.command) st pid id =
  ANSITerminal.(print_string [green] "\nNext Player!\n");
  let st = State.set_id st id in
  match action with 
  | Draw -> 
    let draw_state = 
      (state_from_result (State.draw st (State.get_curr_id st))) in
    if id = 0 then ANSITerminal.(print_string [yellow] 
                                   ("Player " ^ pid ^ " draws " ^ 
                                    State.next_draw st ^ "\n"))
    else ANSITerminal.(print_string [yellow] ("Player " ^ pid ^ " draws\n"));
    print_hands draw_state id pid;
    play_discard draw_state 
  | Eat -> if id = 0 then play_eat st else play_eat_bot st
  | Bump -> begin
      match (State.bump id st) with 
      | Legal t -> ANSITerminal.(print_string 
                                   [yellow] ("Player " ^ pid ^ " bumps " ^ 
                                             (Deck.format 
                                                (State.last_discarded st)) ^ 
                                             "\n"));
        print_hands t id pid;
        play_discard t
      | Illegal -> ANSITerminal.(print_string [red] "\nERROR!\n");
        print_endline "You cannot bump. Please enter a valid draw action."; 
        play_turn st
    end
  | Discard tile_phrase -> ANSITerminal.(print_string [red] "\nERROR!\n");
    print_endline ("You cannot discard before drawing a tile. " ^
                   "Please enter a valid draw action."); 
    play_turn st
  | Win -> if not (State.check_pair (State.win_hand st id true)
                     (State.win_hand st id true)) then 
      begin
        ANSITerminal.(print_string [red] "\nERROR!\n");
        print_endline "You do not have a winning hand. :("; 
        play_turn st 
      end 
    else if (id = 0) then 
      begin
        ANSITerminal.(print_string [green] "Congratulations, you won! :)\n");
        print_endline "Your hand was: ";
        print_endline (String.concat ", " 
                         (List.map Deck.format (State.win_hand st id true)));
        Stdlib.exit 0 
      end
    else begin
      print_endline ("Player " ^ pid ^ " wins. Better luck next time!");
      print_endline "Their hand was: ";
      print_endline (String.concat ", " 
                       (List.map Deck.format (State.win_hand st id true)));
      Stdlib.exit 0 
    end
  | Help -> print_string help; 
    play_turn st
  | Quit -> print_endline "Thanks for playing! See you next time.";
    Stdlib.exit 0;
  | Start -> ANSITerminal.(print_string [red] "\nERROR!\n");
    print_endline ("You cannot start a new game because a game is already " ^
                   "running. If you want to start a new game, please quit " ^ 
                   "this game."); 
    play_turn st
  | Pass -> ANSITerminal.(print_string [red] "\nERROR!\n");
    print_endline ("You cannot pass when it is your turn"); 
    play_turn st
  | Gang -> begin 
      match (State.gang id st) with 
      | Legal t -> ANSITerminal.(print_string [yellow] 
                                   ("Player " ^ pid ^ " gangs " ^ 
                                    (Deck.format (State.last_discarded st)) ^ 
                                    "\n"));
        print_hands t id pid;
        play_discard t
      | Illegal -> ANSITerminal.(print_string [red] "\nERROR!\n");
        print_endline "You cannot bump. Please enter a valid draw action."; 
        play_turn st
    end

(** [play_eat st] is [st] after the current human player has 
    chosen which formation to use when eating the most recently discarded 
    tile. *)
and play_eat st = 
  let id = State.get_curr_id st in 
  let player_id = string_of_int id in 
  let curr_hand = State.get_current_hand st id in 
  let last_discard = State.last_discarded st in
  let is_valid = State.check_eat curr_hand last_discard in
  match is_valid with 
  | Some lst -> begin 
      print_endline ("You can eat. Which combination would you like to make? " ^
                     "(only enter the number)");
      let num_opts = List.length lst in 
      print_endline (Formatter.format_eat lst "" num_opts);
      print_string "> ";
      match read_line () with 
      | exception End_of_file -> ()
      | num -> try let eat_tiles = State.index lst (int_of_string num) in 
          let new_state = State.eat id st eat_tiles in
          match new_state with 
          | Legal t -> ANSITerminal.(print_string [yellow] 
                                       ("Player " ^ player_id ^ " eats " ^ 
                                        (Deck.format (State.last_discarded st)) 
                                        ^ "\n"));
            print_hands t id player_id;
            play_discard t
          | Illegal -> print_endline "This option is not allowed.";
            play_turn st 
        with 
        | e -> ANSITerminal.(print_string [red]
                               "\nERROR!\n");
          print_endline ("You typed an invalid action. " ^
                         "Please enter a valid draw action.");
          play_turn st
    end
  | None -> ANSITerminal.(print_string [red] "\nERROR!\n");
    print_endline "You cannot eat. Please enter a valid draw action.";
    play_turn st

(** [play_eat_bot st] is [st] after the current auto player 
    has eaten the most recent tile. *)
and play_eat_bot st = 
  let id = State.get_curr_id st in 
  let player_id = string_of_int id in 
  let curr_hand = State.get_current_hand st id in 
  let last_discard = State.last_discarded st in
  let is_valid = State.check_eat curr_hand last_discard in
  match is_valid with 
  | Some lst -> let eat_tiles = State.index lst 0 in 
    let new_state = State.eat id st eat_tiles in
    begin
      match new_state with 
      | Legal t -> ANSITerminal.(print_string [yellow] 
                                   ("Player " ^ player_id ^ " eats " ^ 
                                    (Deck.format (State.last_discarded st)) 
                                    ^ "\n"));
        play_discard t
      | Illegal -> failwith "bad bot"
    end
  | None -> failwith "bat bot"

(** [discard_command action st pid id] is [st] after the player[id]
    has discarded a card specified by [action]*)
and discard_command (action : Command.command) st pid id = 
  match action with 
  | Draw -> ANSITerminal.(print_string [red]
                            "\nERROR!\n");
    print_endline "You cannot draw. Please choose a tile to discard.";
    play_discard st 
  | Eat -> ANSITerminal.(print_string [red]
                           "\nERROR!\n");
    print_endline "You cannot eat. Please choose a tile to discard.";
    play_discard st 
  | Bump -> ANSITerminal.(print_string [red]
                            "\nERROR!\n");
    print_endline "You cannot bump. Please choose a tile to discard.";
    play_discard st 
  | Discard tile_phrase -> 
    let discard = Command.tile_from_phrase tile_phrase in
    let discard_st = (state_from_result (State.discard discard st)) in
    ANSITerminal.(print_string [yellow] ("Player " ^ pid ^ " discards " ^ 
                                         (Deck.format discard) ^ "\n"));
    print_hands discard_st id pid;
    if (State.get_curr_id discard_st < 2) then 
      play_turn discard_st else play_between discard_st
  | Win -> if not (State.check_pair (State.win_hand st id false) 
                     (State.win_hand st id false)) then 
      begin
        ANSITerminal.(print_string [red] "\nERROR!\n");
        print_endline "You do not have a winning hand. :("; 
        play_discard st
      end 
    else if (id = 0) then 
      begin
        ANSITerminal.(print_string [green] "Congratulations, you won! :)\n");
        print_endline "Your hand was: ";
        print_endline (String.concat ", " 
                         (List.map Deck.format (State.win_hand st id false)));
        Stdlib.exit 0 
      end
    else 
      begin
        print_endline ("Player " ^ pid ^ " wins. Better luck next time!");
        print_endline "Their hand was: ";
        print_endline (String.concat ", " 
                         (List.map Deck.format (State.win_hand st id false)));
        Stdlib.exit 0 
      end

  | Help -> print_string help; 
    play_discard st
  | Quit -> print_endline "Thanks for playing! See you next time.";
    Stdlib.exit 0;
  | Start ->  ANSITerminal.(print_string [red]
                              "\nERROR!\n");
    print_endline ("You cannot start a new game because a game is already " ^
                   "running. If you want to start a new game, please quit " ^ 
                   "this game.");
    play_discard st
  | Pass -> ANSITerminal.(print_string [red]
                            "\nERROR!\n");
    print_endline "You cannot pass. Please choose a tile to discard.";
    play_discard st 
  | Gang -> match State.gang_hand id st with 
    | Legal st' -> ANSITerminal.(print_string [yellow] 
                                   ("Player " ^ pid ^ " gangs " ^ "\n"));
      print_hands st' id pid;
      play_discard st'
    | Illegal ->  
      ANSITerminal.(print_string [red]
                      "\nERROR!\n");
      print_endline "You cannot gang. Please choose a tile to discard.";
      play_discard st

(** [play_between st] is [st] after the current human player has chosen
    an action and it has been applied to [st] *)
and play_between st = 
  print_endline ("Your current hand is");
  print_endline ((Formatter.format_current st 0) ^ "\n");
  print_endline ("Would you like to perform a special action or pass?");
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | command -> 
    try let action = Command.parse command in
      begin
        match action with 
        | Bump -> if State.bump 0 st = Illegal then 
            begin
              ANSITerminal.(print_string [red] "\nERROR!\n");
              print_endline"You cannot bump. Please enter a valid action.";
              play_between st 
            end
          else play_command action st "0" 0
        | Win -> if not (State.check_pair (State.win_hand st 0 true) 
                           (State.win_hand st 0 false)) then 
            begin
              ANSITerminal.(print_string [red] "\nERROR!\n");
              print_endline "You do not have a winning hand. :(\n" ;
              play_between st 
            end else 
            ANSITerminal.(print_string [green] "Congratulations, you won! :)\n");
          print_endline "Your hand was: ";
          print_endline (String.concat ", " 
                           (List.map Deck.format (State.win_hand st 0 true)));
          Stdlib.exit 0 
        | Help -> print_string help; 
          play_between st
        | Quit -> print_endline "Thanks for playing! See you next time.";
          Stdlib.exit 0;
        | Pass -> play_turn st
        | Gang -> if State.gang 0 st = Illegal then 
            begin
              ANSITerminal.(print_string [red] "\nERROR!\n");
              print_endline"You cannot gang. Please enter a valid action.";
              play_between st 
            end
          else play_command action st "0" 0
        | _ -> ANSITerminal.(print_string [red] "\nERROR!\n");
          print_endline "That is not a valid action at this time.";
          play_between st
      end
    with 
    | Command.Empty -> ANSITerminal.(print_string [red]
                                       "\nERROR!\n");
      print_endline ("You typed an empty action. " ^
                     "Please enter a new non-empty draw action.");
      play_between st
    | Command.Malformed -> ANSITerminal.(print_string [red]
                                           "\nERROR!\n");
      print_endline ("You typed an invalid action. " ^
                     "Please enter a valid draw action.");
      play_between st 

(** [play_discard st] is [st] after the current player has discarded a card of 
    their choice*)
and play_discard st  = 
  let id = State.get_curr_id st in 
  let player_id = string_of_int id in
  if (id = 0) then
    begin
      print_endline ("Pick a card to discard (or win), Player " ^ player_id);
      print_string "> ";
      match read_line () with 
      | exception End_of_file -> () 
      | command -> 
        try let action = Command.parse command in
          discard_command action st player_id id
        with 
        | Command.Empty -> ANSITerminal.(print_string [red]
                                           "\nERROR!\n");
          print_endline ("You typed an empty action. Please enter a new " ^
                         "non-empty action.");
          play_discard st 
        | Command.Malformed -> ANSITerminal.(print_string [red]
                                               "\nERROR!\n");
          print_endline ("You typed an invalid action. Please enter a " ^
                         "valid action.");
          play_discard st
    end
  else let discards = Autoplayer.get_discard_moves st id in
    match discards with 
    | h ::t -> discard_command h st player_id id
    | [] -> failwith "no discards"

(** [start_game command] is the terminal output after parsing [command].
    It is an initial state for the mahjong game if [command] is Start. *)
let rec start_game command = 
  try let action = Command.parse command in 
    match action with 
    | Start -> begin
        print_endline "Please choose your game difficulty (easy or hard).";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | level -> try let difficulty = State.parse_difficulty level in
            let start_state = State.init_state difficulty in
            let draw_state = State.draw start_state 
                (State.get_curr_id start_state) in
            let player_id = string_of_int (State.get_curr_id start_state) in
            ANSITerminal.(print_string [green] "\nStarting the game.\n");
            ANSITerminal.(print_string [yellow] 
                            ("Player " ^ player_id ^ " draws " ^ 
                             State.next_draw start_state ^ "\n"));
            print_endline "Your hand is now";
            print_endline ((Formatter.format_current 
                              (state_from_result draw_state) 0) ^ "\n");
            print_endline ("Your displayed hand is");
            print_endline ((Formatter.format_display 
                              (state_from_result draw_state) 0) ^ "\n");
            play_discard (state_from_result draw_state) 
          with 
          | State.UnknownDifficulty -> ANSITerminal.(print_string [red]
                                                       "\nERROR!\n");
            print_endline ("You typed an unknown difficulty. Please enter a " ^ 
                           "valid difficulty.");
            start_game command
      end
    | Help -> print_string help;
      print_string "> ";
      (match read_line () with 
       | exception End_of_file -> () 
       | command -> start_game command)
    | Quit -> print_endline "Thanks for playing! See you next time.";
      Stdlib.exit 0;
    | _ -> ANSITerminal.(print_string [red]
                           "\nERROR!\n");
      print_endline "You typed an invalid action. Please enter \"start\" 
      to start the game.";
      print_string "> ";
      (match read_line () with 
       | exception End_of_file -> ()
       | new_command -> start_game new_command)
  with
  | Command.Empty -> ANSITerminal.(print_string [red]
                                     "\nERROR!\n");
    print_endline "You typed an empty action. Please enter \"start\" 
    to start the game.";
    print_string "> ";
    (match read_line () with 
     | exception End_of_file -> ()
     | new_command -> start_game new_command)
  | Command.Malformed -> ANSITerminal.(print_string [red]
                                         "\nERROR!\n");
    print_endline "You typed an invalid action. Please enter \"start\" 
    to start the game.";
    print_string "> ";
    (match read_line () with 
     | exception End_of_file -> ()
     | new_command -> start_game new_command)

(** [main ()] prompts for the game to play, then starts it. *)
let main () = 
  ANSITerminal.(print_string [green]
                  "\nWelcome to the world of Mahjong.\n");
  print_endline ("Type \"start\" to begin playing! If you need help or want to"
                 ^ " learn the rules of the game, type \"help\".\n");
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | command -> start_game command

(** Execute the game engine. *)
let () = main ()