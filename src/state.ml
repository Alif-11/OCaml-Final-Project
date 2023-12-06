(* (** The signature of Game State*) module type GameState = sig type t (**
   Representation type of Game State *)

   val initialize : t (** Create a starting state *)

   val time : t -> int (** Time allotted for round in seconds *)

   val num_words : t -> int (** Number of words to be generated *)

   val difficulty : t -> int (** Difficulty level of words generated *)

   val health : t -> int (** Current health *)

   val max_health : t -> int (** Current max health *)

   val cur_level : t -> int (** Number of rounds player has passed so far *)

   val health_lost : int -> int -> int -> t -> t (** Calculates health lost
   given time remaining, accuracy, words remaining *)

   val adjust_level : t -> t (** Adjust level depending on number of rounds and
   *) end

   module NormalGame : GameState = struct type t = { time : int; num_words :
   int; diff : int; hp : int; max_hp : int; cur_lvl : int; }

   let initialize = { time = 120; num_words = 50; diff = 1; hp = 100; max_hp =
   100; cur_lvl = 0; }

   let time gs = gs.time let num_words gs = gs.num_words let difficulty gs =
   gs.diff let health gs = gs.hp let max_health gs = gs.hp let cur_level gs =
   gs.hp

   let health_lost time_left missed words_left gs = let damage_taken = time_left
   - missed - words_left in { gs with hp = min (gs.hp + damage_taken) gs.max_hp
   }

   let adjust_level gs = { gs with cur_lvl = gs.cur_lvl + 1; num_words =
   gs.num_words + 10 } end *)

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

(* Mutable version of the below class. *)
module NormalGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0

  let initialize () =
    _time := 120;
    _num_words := 50;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level

  let health_lost time_left missed words_left =
    _health := min (time_left - missed - words_left) (max_health ())

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 10
end
<<<<<<< HEAD

(* (** The signature of Game State*) module type GameState = sig type t (**
   Representation type of Game State *)

   val initialize : t (** Create a starting state *)

   val time : t -> int (** Time allotted for round in seconds *)

  let health_lost time_left missed words_left gs = 
    let damage_taken = time_left - missed * 5 - words_left * 5 in
    {gs with hp = min (gs.hp + damage_taken) (gs.max_hp)}
   val num_words : t -> int (** Number of words to be generated *)

   val difficulty : t -> int (** Difficulty level of words generated *)

   val health : t -> int (** Current health *)

   val max_health : t -> int (** Current max health *)

   val cur_level : t -> int (** Number of rounds player has passed so far *)

   val health_lost : int -> int -> int -> t -> t (** Calculates health lost
   given time remaining, accuracy, words remaining *)

   val adjust_level : t -> t (** Adjust level depending on number of rounds and
   *) end

   module NormalGame : GameState = struct type t = { time : int; num_words :
   int; diff : int; hp : int; max_hp : int; cur_lvl : int; }

   let initialize = { time = 120; num_words = 50; diff = 1; hp = 100; max_hp =
   100; cur_lvl = 0; }

   let time gs = gs.time let num_words gs = gs.num_words let difficulty gs =
   gs.diff let health gs = gs.hp let max_health gs = gs.hp let cur_level gs =
   gs.hp

   let health_lost time_left missed words_left gs = let damage_taken = time_left
   - missed - words_left in { gs with hp = min (gs.hp + damage_taken) gs.max_hp
   }

   let adjust_level gs = { gs with cur_lvl = gs.cur_lvl + 1; num_words =
   gs.num_words + 10 } end *)
=======
>>>>>>> ba49209fea02d6047efc1a911636256d00f4a017
