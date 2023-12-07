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
  val _num_items : int ref

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
  (** Current player score*)

  val num_items : unit -> int
  (** Number of items generated each time*)

  val health_lost : int -> int -> int -> unit
  (** Calculates health lost given time remaining, words wrong and words
      remaining *)

  val adjust_level : unit -> unit
  (** Adjust level depending on number of rounds *)

  val decrement_level : unit -> unit
  (** Decrease level *)

  val add_score : int -> unit
  (** add score based on number of correct words as passed in*)
end

module NormalGameMutable : GameStateMutable
