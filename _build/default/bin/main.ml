let round start_time = 
  let cur_time = Unix.gettimeofday () in
  let time_passed = 
      int_of_float (Float.round ((cur_time -. start_time) *. 100.0)) in
  let sec = time_passed / 100 mod 60 in
  let string_sec = if sec >= 10 then string_of_int sec 
    else "0" ^ string_of_int sec in
  let min = time_passed / 6000 in
  let time_string = string_of_int min ^ ":" ^ string_sec in
  print_endline time_string;
  Graphics.moveto 0 0;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 300 300;
  Graphics.set_color Graphics.black;
  Graphics.draw_string time_string

let () = 
    Graphics.open_graph " 3000x2000+0+0";
    let start_time = Unix.gettimeofday () in
  while true do
    Unix.sleep 1;
    round start_time;
  done