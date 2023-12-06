open TypeGame

type round_state = {
  pos : int * int;
  mutable str : string;
  fin : bool;
  mutable typed : int;
  mutable right : int;
  mutable wrong : int;
  mutable words : string list;
}

let time_passed start_time cur_time =
  int_of_float (Float.round ((cur_time -. start_time) *. 100.0))

(*for keeping spacing relatively consistent*)
let new_line = function
  | x, y -> if x < 800 then () else Graphics.moveto 100 (y - 25)

(*things happening at each tick of a round*)
let round_tick start_time time_allotted (state : round_state) =
  let cur_time = Unix.gettimeofday () in
  let time_passed = time_passed start_time cur_time in
  let frac_sec = time_passed mod 100 in
  let string_frac_sec =
    if frac_sec < 10 then "0" ^ string_of_int frac_sec
    else string_of_int frac_sec
  in
  let sec = time_passed / 100 mod 60 in
  let string_sec =
    if sec >= 10 then string_of_int sec else "0" ^ string_of_int sec
  in
  let min = time_passed / 6000 in
  let time_string =
    string_of_int min ^ ":" ^ string_sec ^ "." ^ string_frac_sec
  in
  Graphics.moveto 100 900;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 100 900 100 30;
  Graphics.set_color Graphics.black;
  Graphics.draw_string time_string;
  if time_passed >= time_allotted then { state with fin = true }
  else
    let x_pos = fst state.pos in
    let y_pos = snd state.pos in
    match Graphics.key_pressed () with
    | true ->
        Graphics.moveto x_pos y_pos;
        let key = Graphics.read_key () in
        Graphics.draw_char key;
        if key = ' ' || key = Char.chr 13 then (
          new_line (Graphics.current_point ());
          state.typed <- state.typed + 1;
          (match state.words with
          | [] -> state.wrong <- state.wrong + 1
          | h :: t ->
              if h = state.str then state.right <- state.right + 1
              else state.wrong <- state.wrong + 1;
              state.words <- t);
          state.str <- "";
          if key = ' ' then { state with pos = Graphics.current_point () }
          else { state with fin = true })
        else
          {
            state with
            pos = Graphics.current_point ();
            str = state.str ^ String.make 1 key;
          }
    | false -> state

let round gs =
  Graphics.moveto 100 800;
  let word_list =
    Game.generate_sequence
      (Game.word_bag_t (State.NormalGameMutable.difficulty gs))
      (State.NormalGameMutable.num_words gs)
  in
  let words_given = List.length word_list in
  let rec print_words = function
    | [] -> ()
    | h :: t ->
        Graphics.draw_string (h ^ " ");
        new_line (Graphics.current_point ());
        print_words t
  in
  let start_time = Unix.gettimeofday () in
  let finished = ref false in
  let rs_tick =
    ref
      {
        pos = (100, 500);
        str = " ";
        fin = false;
        typed = 0;
        right = 0;
        wrong = 0;
        words = word_list;
      }
  in
  Graphics.moveto 100 800;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "This round you will have:";
  Graphics.moveto 100 770;
  Graphics.draw_string
    (string_of_int (State.NormalGameMutable.num_words ())
    ^ " words in "
    ^ string_of_int (State.NormalGameMutable.time ())
    ^ " seconds.");
  Graphics.moveto 100 100;
  Graphics.draw_string "Are you ready? <PRESS ANY KEY TO START ROUND>";
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 800;
  print_words word_list;
  while !finished = false do
    Unix.sleepf 0.001;
    rs_tick := round_tick start_time 6000 !rs_tick;
    finished := !rs_tick.fin
  done;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 800;
  let time_passed = time_passed start_time (Unix.gettimeofday ()) in
  let accuracy = !rs_tick.right * 100 / !rs_tick.typed in
  Graphics.draw_string
    ("Your accuracy was " ^ string_of_int accuracy ^ "% with "
    ^ string_of_int !rs_tick.right
    ^ " out of " ^ string_of_int words_given ^ " words typed correctly.");
  let words_left = Int.abs words_given - !rs_tick.typed in
  State.NormalGameMutable.health_lost time_passed !rs_tick.wrong words_left;
  State.NormalGameMutable.add_score !rs_tick.right;
  let cur_health = !State.NormalGameMutable._health in
  Graphics.moveto 100 770;
  Graphics.draw_string
    ("You are now at " ^ string_of_int cur_health ^ " health!");
  Graphics.moveto 100 740;
  if cur_health > 0 then
    ( true,
      Graphics.draw_string
        ("Your current score is "
        ^ string_of_int !State.NormalGameMutable._score
        ^ ".") )
  else (false, Graphics.draw_string "You died! Better luck next time.")

let () =
  (*Game initialization*)
  Graphics.open_graph " 1000x1000+0+0";
  Graphics.moveto 100 800;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Rules : ";
  Graphics.moveto 100 775;
  Graphics.draw_string "1. You lose health for getting words wrong";
  Graphics.draw_string
    "2. Once you press space, we consider that a new word! You can't go back \
     to fix your mistakes :)";
  Graphics.moveto 100 750;
  Graphics.draw_string
    "3. If you don't finish, then any words left over will also cost you some \
     health :)";
  Graphics.moveto 100 725;
  Graphics.draw_string
    "4. If you have time left over, you can regain some health";
  Graphics.moveto 100 700;
  Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  let gs = ref (State.NormalGameMutable.initialize ()) in
  let playing = ref true in
  (*Game loop*)
  while !playing do
    playing := fst (round !gs);
    Graphics.moveto 100 100;
    Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
    ignore (Graphics.wait_next_event [ Key_pressed ]);
    State.NormalGameMutable.adjust_level ();
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 1000 1000
  done
