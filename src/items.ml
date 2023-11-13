open State

(* Signature detailing basic properties of any item. *)
module type Items = sig
  (* How likely said item is to be dropped. Its value can range from 1 (1% drop
     rate) to 100 (100% drop rate). *)
  val drop_chance : int

  (* Name of the item. *)
  val item_name : string

  (* Description of the item's effects. Used to insert comedic text. *)
  val item_description : string

  (* Modified NormalGameMutable in state.ml to cause effects to occur. *)
  val effects : unit -> unit
end

module Apple : Items = struct
  let drop_chance = 30
  let item_name = "Apple"
  let item_description = "Tastes like a banana."

  let effects () =
    let hp = NormalGameMutable._health in
    hp := !hp + 5
end

module Banana : Items = struct
  let drop_chance = 30
  let item_name = "Banana"
  let item_description = "Tastes like an apple."

  let effects () =
    let hp = NormalGameMutable._health in
    hp := !hp + 5
end

module BrokenClock : Items = struct
  let drop_chance = 10
  let item_name = "Broken Clock"
  let item_description = "A broken clock is usually 10 seconds off."

  let effects () =
    let time = NormalGameMutable._time in
    time := !time + 10
end

module EdibleClock : Items = struct
  let drop_chance = 5
  let item_name = "Let Them Eat Clock"

  let item_description =
    "It should tell the time ... but the clock arms never move, and they sure \
     do look tasty..."

  let effects () =
    let hp = NormalGameMutable._health in
    let time = NormalGameMutable._time in
    hp := !hp + 10;
    time := !time + 15
end

module Chaos : Items = struct
  let drop_chance = 1
  let item_name = "???"
  let item_description = "Don't."

  let effects () =
    let hp = NormalGameMutable._health in
    let time = NormalGameMutable._time in
    if Random.int 2 = 0 then (
      hp := 0;
      time := 0)
    else (
      hp := 250;
      time := 90)
end
