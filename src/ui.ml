
let timer = 
    let start_time = Unix.gettimeofday () in
    fun () -> let cur = Unix.gettimeofday () in
    let time_passed = int_of_float (Float.round ((cur -. start_time) *. 100.0)) in
    let hundredths_sec = time_passed mod 100 in
    let sec = time_passed mod 600 in
    let min = time_passed / 600 in
    string_of_int min ^ ":" ^ 
    string_of_int sec ^ "." ^ string_of_int hundredths_sec
let draw_timer () = 
  let time = timer () in
  Graphics.draw_string time
    
