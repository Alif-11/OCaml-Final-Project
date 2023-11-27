open State

type name =
  | Apple
  | Banana
  | BrokenClock
  | EdibleClock
  | Chaos

type rarity =
  | Common
  | Rare
  | Epic
  | Undiscovered

type item = name * rarity * string * string * (unit -> unit)

let apple_effect () =
  let hp = NormalGameMutable._health in
  hp := !hp + 5

let apple : item =
  (Apple, Common, "Banapple", "Tastes like a banana.", apple_effect)

let banana_effect () =
  let hp = NormalGameMutable._health in
  hp := !hp + 5

let banana : item =
  (Banana, Common, "Appanana", "Tastes like an apple.", banana_effect)

let broken_clock_effect () =
  let time = NormalGameMutable._time in
  time := !time + 10

let broken_clock : item =
  ( BrokenClock,
    Rare,
    "A Broken Clock",
    "It's right twice a day, even though it's ten seconds off.",
    broken_clock_effect )

let edible_clock_effect () =
  let hp = NormalGameMutable._health in
  let time = NormalGameMutable._time in
  hp := !hp + 10;
  time := !time + 15

let edible_clock : item =
  ( EdibleClock,
    Epic,
    "Let Them Eat Clock",
    "It ... kinda looks like cake??",
    edible_clock_effect )

let chaos_effect () =
  let hp = NormalGameMutable._health in
  let time = NormalGameMutable._time in
  if Random.int 2 = 0 then (
    hp := 0;
    time := 0)
  else (
    hp := 250;
    time := 90)

let chaos : item = (Chaos, Undiscovered, "???", "Don't.", chaos_effect)

module type ItemBag = sig
  type items

  val obtain_item : unit -> item
end

module ArrayItemBag : ItemBag = struct
  type items = {
    common : item array;
    rare : item array;
    epic : item array;
    undiscovered : item array;
  }

  let itemlist =
    {
      common = [| apple; banana |];
      rare = [| broken_clock |];
      epic = [| edible_clock |];
      undiscovered = [| chaos |];
    }

  let obtain_item () =
    Random.self_init ();
    let i = itemlist in
    let chosen_number = Random.int 100 in
    if chosen_number < 50 then
      let lst = i.common in
      Array.get lst (Random.int (Array.length lst))
    else if chosen_number < 80 then
      let lst = i.rare in
      Array.get lst (Random.int (Array.length lst))
    else if chosen_number < 95 then
      let lst = i.epic in
      Array.get lst (Random.int (Array.length lst))
    else
      let lst = i.undiscovered in
      Array.get lst (Random.int (Array.length lst))
end
