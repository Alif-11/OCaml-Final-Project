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
    let curScore = state.score in
    match Graphics.key_pressed () with
    |true ->  
      Graphics.moveto x_pos y_pos;
      let key = Graphics.read_key () in
      Graphics.draw_char key;
      if key = ' ' then new_line (Graphics.current_point ());
      if key = ' ' then {state with pos = Graphics.current_point (); str = " "; score = curScore}
      else 
        {state with pos = Graphics.current_point (); 
        str = curStr ^ String.make 1 key} 
    |false -> state

  

let round ()= 
  Graphics.moveto 100 800;
  let word_list = Game.generate_sequence Game.word_bag 50 in
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
    match cursorX with | {contents = integer} -> let xpos = integer in 
    match cursorY with | {contents = integer} -> let ypos = integer in
    match str with | {contents = string} -> let curStr = string in
    match score with | {contents = integer} -> let curScore = integer in
    match fin with | {contents = boolean} -> let isFin = boolean in
    let bruh = round_tick start_time 6000 {pos = (xpos, ypos); str = curStr; score = curScore; fin = isFin} in
    cursorX := fst bruh.pos;
    cursorY := snd bruh.pos;
    str := bruh.str;
    score := bruh.score;
    fin := bruh.fin
  done

let () = 
  Graphics.open_graph " 1000x1000+0+0";
  round ();