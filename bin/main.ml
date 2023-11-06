open TypeGame

type round_state = 
  {pos : int * int; str : string; score : int; fin : bool}

let new_line = function
|(x, y) -> if x < 800 then ()
  else Graphics.moveto 100 (y - 25)

let round_tick start_time time_allotted (state : round_state) = 
  let cur_time = Unix.gettimeofday () in
  let time_passed = 
      int_of_float (Float.round ((cur_time -. start_time) *. 100.0)) in
  let frac_sec = time_passed mod 100 in
  let string_frac_sec = if frac_sec < 10 then "0" ^ string_of_int frac_sec
    else string_of_int frac_sec in
  let sec = time_passed / 100 mod 60 in
  let string_sec = if sec >= 10 then string_of_int sec 
    else "0" ^ string_of_int sec in
  let min = time_passed / 6000 in
  let time_string = 
    string_of_int min ^ ":" ^ string_sec ^ "." ^ string_frac_sec in
  Graphics.moveto 100 900;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 100 900 100 30;
  Graphics.set_color Graphics.black;
  Graphics.draw_string time_string;
  if time_passed >= time_allotted then
    {state with fin = true}
  else
    let x_pos = fst state.pos in
    let y_pos = snd state.pos in
    let curStr = state.str in
    match Graphics.key_pressed () with
    |true ->  
      Graphics.moveto x_pos y_pos;
      let key = Graphics.read_key () in
      Graphics.draw_char key;
      if key = ' ' then new_line (Graphics.current_point ());
      if key = Char.chr 13 then {state with fin = true}
      else 
        {state with pos = Graphics.current_point (); 
        str = curStr ^ String.make 1 key} 
    |false -> state

  

let round gs = 
  Graphics.moveto 100 800;
  let word_list = Game.generate_sequence 
    (Game.word_bag_t (State.NormalGame.difficulty gs))
    (State.NormalGame.num_words gs) in
  let rec print_words = function
  |[] -> ();
  |h :: t -> Graphics.draw_string (h ^ " ");
    new_line (Graphics.current_point ());
    print_words t in
  print_words word_list;
  let start_time = Unix.gettimeofday () in
  let cursorX = ref 100 in
  let cursorY = ref 500 in
  let str = ref " " in
  let score = ref 0 in
  let fin = ref false in
  while fin = ref false do
    Unix.sleepf 0.001;
    let graphics_state = round_tick start_time 6000 {pos = (!cursorX, !cursorY); str = !str; score = !score; fin = !fin} in
    cursorX := fst graphics_state.pos;
    cursorY := snd graphics_state.pos;
    str := graphics_state.str;
    score := graphics_state.score;
    fin := graphics_state.fin
  done


let () = 
  Graphics.open_graph " 1000x1000+0+0";
  Graphics.moveto 100 800;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Rules : ";
  Graphics.moveto 100 775; 
  Graphics.draw_string "1. You lose health for getting words wrong";
  Graphics.draw_string "2. Once you press space, we consider that a new word! You can't go back to fix your mistakes :)";
  Graphics.moveto 100 750;
  Graphics.draw_string "3. If you don't finish, then any words left over will also cost you some health :)";
  Graphics.moveto 100 725;
  Graphics.draw_string "4. If you have time left over, you can regain some health";
  Graphics.moveto 100 700;
  Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
  ignore (Graphics.wait_next_event [Key_pressed]);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  let gs = State.NormalGame.initialize in
  while true do
    Graphics.moveto 100 800;
    Graphics.set_color Graphics.black;
    Graphics.draw_string "Are you ready? <PRESS ANY KEY TO START ROUND>";
    ignore (Graphics.wait_next_event [Key_pressed]);
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 1000 1000;
    Graphics.set_color Graphics.black;
    round gs;
    Graphics.moveto 100 100;
    Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
    ignore (Graphics.wait_next_event [Key_pressed]);
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 1000 1000;
  done