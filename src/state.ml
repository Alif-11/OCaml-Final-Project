
(** The signature of Game State*)
module type GameState = sig
  type t
  (** Representation type of Game State *)

  val initialize : t
  (** Create a starting state *)

  val time : t -> int
  (** Time allotted for round in seconds *)

  val num_words : t -> int
  (** Number of words to be generated *)

  val difficulty : t -> int
  (** Difficulty level of words generated *)

  val health : t -> int
  (** Current health *)

  val max_health : t -> int
  (** Current max health *)

  val cur_level : t -> int
  (** Number of rounds player has passed so far *)

  val health_lost : int -> int -> int -> t -> t
  (** Calculates health lost given time remaining, accuracy, words remaining *)

  val adjust_level : t -> t
  (** Adjust level depending on number of rounds and *)
end

module NormalGame : GameState = struct
  type t = {time : int; num_words : int; diff : int; hp : int; max_hp : int; cur_lvl : int;}

  let initialize = 
    {time = 120; num_words = 50; diff = 1; hp = 100; max_hp = 100; cur_lvl = 0}

  let time gs = gs.time

  let num_words gs = gs.num_words

  let difficulty gs = gs.diff

  let health gs = gs.hp

  let max_health gs = gs.hp

  let cur_level gs = gs.hp

  let health_lost time_left missed words_left gs = 
    let damage_taken = time_left - missed * 5 - words_left * 5 in
    {gs with hp = min (gs.hp + damage_taken) (gs.max_hp)}

  let adjust_level gs = {gs with cur_lvl = gs.cur_lvl + 1; num_words = gs.num_words + 10}

end