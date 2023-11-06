(** The signature of Game State*)
module type GameState = sig
    type t
    (** Representation type of Game State *)

    val initialize : t
    (** Create an initial game state*)

    val time : t -> int
    (** Time allotted for round in seconds *)

    val num_words : t -> int
    (** Number of words to be generated *)

    val difficulty : t -> int
    (** Difficulty level of words generated *)

    val health : t -> int
    (** Current health *)

    val max_health : t -> int
    (** Current max health*)

    val cur_level : t -> int
    (** Number of rounds player has passed so far *)

    val health_lost : int -> int -> int -> t -> t
    (** Calculates health lost given time remaining, accuracy, words remaining *)

    val adjust_level : t -> t
    (** Adjust level depending on number of rounds and *)
end

module NormalGame : GameState