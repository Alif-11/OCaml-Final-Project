(* The names of all possible items in the game. *)
type name

(* How rare this item is. *)
type rarity

(* The type of every item in the game. Includes a [name], a [rarity], a display
   name as a [string], an item description as a [string], and an effect as a
   (unit -> unit). *)
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

(* The signature for the module that randomly selects an item. *)
module type ItemBag = sig
  (* The data representation for pulling an item. *)
  type items

  (* Randomly obtain an item. *)
  val obtain_item : unit -> item
end

module ArrayItemBag : ItemBag
