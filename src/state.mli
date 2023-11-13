module type GameStateMutable = sig
  val initialize :
    (* int list (* if we want to dynamically initialize the game state *) -> *)
    unit ->
    unit
  (** Set starting state values *)

  val _time : int ref
  val _num_words : int ref
  val _difficulty : int ref
  val _health : int ref
  val _max_health : int ref
  val _cur_level : int ref

  val time : unit -> int
  (** Time allotted for round in seconds *)

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

  val health_lost : int -> int -> int -> unit
  (** Calculates health lost given time remaining, accuracy, words remaining *)

  val adjust_level : unit -> unit
  (** Adjust level depending on number of rounds *)
end

module NormalGameMutable : GameStateMutable

(* (** The signature of Game State*) module type GameState = sig type t (**
   Representation type of Game State *)

   val initialize : t (** Create an initial game state*)

   val time : t -> int (** Time allotted for round in seconds *)

   val num_words : t -> int (** Number of words to be generated *)

   val difficulty : t -> int (** Difficulty level of words generated *)

   val health : t -> int (** Current health *)

   val max_health : t -> int (** Current max health*)

   val cur_level : t -> int (** Number of rounds player has passed so far *)

   val health_lost : int -> int -> int -> t -> t (** Calculates health lost
   given time remaining, accuracy, words remaining *)

   val adjust_level : t -> t (** Adjust level depending on number of rounds and
   *) end

   module NormalGame : GameState *)
