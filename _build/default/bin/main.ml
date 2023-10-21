let round_tick start_time cursorX cursorY= 
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
  let time_string = string_of_int min ^ ":" ^ string_sec ^ "." ^ string_frac_sec in
  print_endline time_string;
  Graphics.moveto 0 1375;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 1375 100 30;
  Graphics.set_color Graphics.black;
  Graphics.draw_string time_string;
  match Graphics.key_pressed () with
  |true ->  
    Graphics.moveto cursorX cursorY;
    print_endline (string_of_int cursorX ^ " " ^ string_of_int cursorY);
    Graphics.draw_char (Graphics.read_key ());
    Graphics.current_point ()
  |false -> (cursorX, cursorY)

let round ()= 
  let start_time = Unix.gettimeofday () in
  let cursorX = ref 0 in
  let cursorY = ref 1000 in
  while true do
    Unix.sleepf 0.001;
    match cursorX with | {contents = integer} -> let xpos = integer in 
    match cursorY with | {contents = integer} -> let ypos = integer in
    let bruh = round_tick start_time xpos ypos in
    cursorX := fst bruh;
    cursorY := snd bruh;
  done

let () = 
  Graphics.open_graph " 1500x1500+0+0";
  round ();