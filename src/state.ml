(** The signature of a mutable version of Game State*)
module type GameStateMutable = sig
  val initialize :
    (* int list (* if we want to dynamically initialize the game state *) -> *)
    unit ->
    unit
  (** Set starting state values *)

  val time : unit -> int
  (** Time allotted for round in seconds *)

  val _time : int ref
  val _num_words : int ref
  val _difficulty : int ref
  val _health : int ref
  val _max_health : int ref
  val _cur_level : int ref
  val _score : int ref

  val num_words : unit -> int
  (** Number of words to be generated *)

  val difficulty : unit -> int
  (** Difficulty level of words generated *)

  val health : unit -> int
  (** Current health *)

  val max_health : unit -> int
  (** Current max health *)

  val cur_level : unit -> int
  (** Number of rounds player has passed so far *)

  val score : unit -> int

  val health_lost : int -> int -> int -> unit
  (** Calculates health lost given time remaining, accuracy, words remaining *)

  val adjust_level : unit -> unit
  (** Adjust level depending on number of rounds *)

  val add_score : int -> unit
  (** Adjust score based on number of correct words typed*)
end

(* Mutable version of the below class. *)
module NormalGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0

  let initialize () =
    _time := 120;
    _num_words := 50;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score

  let health_lost time_left missed words_left =
    _health :=
      min
        (!_health + (time_left / 5) - (missed * 5) - (words_left * 5))
        (max_health ())

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 5;
    _time := !_time - 5

  let add_score correct = _score := !_score + (correct * 5)
end
