(* The names of all possible items in the game. *)
type name

(* How rare this item is. *)
type rarity

(* The type of every item in the game. Includes a [name], a [rarity], a display
   name as a [string], an item description as a [string], a stats description as
   a [string], and an effect as a (unit -> unit). *)
type item

(* The Banapple effect. *)
val apple_effect : unit -> unit

(* The Appanana effect. *)
val banana_effect : unit -> unit

(* The Broken Clock effect. *)
val broken_clock_effect : unit -> unit

(* The Edible Clock effect. *)
val edible_clock_effect : unit -> unit

(* The effect of Chaos. *)
val chaos_effect : unit -> unit

(* The effect of the Forgotton Altar (* ominous music plays *). *)
val forgotton_altar_effect : unit -> unit

(* The effect of the Bloody Altar (* less ominous music plays *). *)
val bloody_altar_effect : unit -> unit

(* The signature for the module that randomly selects an item. *)
module type ItemBag = sig
  (* The data representation for pulling an item. *)
  type items

  (* Randomly obtain an item. *)
  val obtain_item : unit -> item
end

module ArrayItemBag : ItemBag

(* Returns the name of the item. *)
val name_to_string : item -> string

(* Returns a series of effect statements, separated by the newline character,
   stating what changes the player received, of the item. *)
val effect_to_string : item -> string list

(* Returns the funny funny flavor text of the item. *)
val flavor_to_string : item -> string

(* Returns the rarity of the item. *)
val rarity_to_string : item -> string
