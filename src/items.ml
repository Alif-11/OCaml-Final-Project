open State

type name =
  | Apple
  | Banana
  | BrokenClock
  | EdibleClock
  | Chaos
  | ForgottenAltar
  | BloodyAltar
  | Obfuscinator
(*| Jetpack | ReverseJetpack*)

type rarity =
  | Common
  | Rare
  | Epic
  | Undiscovered

type item = name * rarity * string * string * ((* string * *) unit -> unit)

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

let forgotton_altar_effect () =
  let hp = NormalGameMutable._health in
  let time = NormalGameMutable._time in
  time := !time / 2;
  hp := !hp * 2

let forgotton_altar : item =
  ( ForgottenAltar,
    Rare,
    "The TIME Altar",
    "This altar is showing clear wear and tear due to the passage of time. It \
     seems to want something from you...",
    forgotton_altar_effect )

let bloody_altar_effect () =
  let hp = NormalGameMutable._health in
  let time = NormalGameMutable._time in
  time := !time * 2;
  hp := !hp / 2

let bloody_altar : item =
  ( BloodyAltar,
    Rare,
    "The BLOOD Altar",
    "This altar is overflowing with blood, but its pulsing for more. It seems \
     to want something from you...",
    forgotton_altar_effect )

let obfuscinator_effect () =
  let hp = NormalGameMutable._health in
  let time = NormalGameMutable._time in
  let tmp = !time in
  time := !hp;
  hp := tmp

let obfuscinator : item =
  ( Obfuscinator,
    Epic,
    "The Obfuscinator!!!",
    "Doofenshmirtz's latest invention! Allows you to make a deal with Time in \
     exchange for your soul.",
    obfuscinator_effect )

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
      rare = [| broken_clock; forgotton_altar; bloody_altar |];
      epic = [| edible_clock; obfuscinator |];
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

let name_to_string (item : item) =
  let _, _, n, _, _ = item in
  n

let effect_to_string (item : item) =
  let i, _, _, _, _ = item in
  match i with
  | Apple -> [ "You gained 5 health."; "It still tastes like banana." ]
  | Banana -> [ "You grained 5 health."; "It still tastes like apple." ]
  | BrokenClock -> [ "You gained 10 seconds."; "This was not a tasty clock." ]
  | EdibleClock ->
      [
        "You gained 10 health.";
        "You gained 15 seconds.";
        "This was a tasty clock.";
      ]
  | Chaos ->
      [ "You were warned..."; "You didn't have much to eat this time around." ]
  | ForgottenAltar ->
      [
        "You gained ??? health.";
        "You lost !!! time.";
        "The concrete material was rather savory.";
      ]
  | BloodyAltar ->
      [
        "You lost ??? health.";
        "You gained !!! time.";
        "It wasn't blood: it was pomegranate juice. ";
      ]
  | Obfuscinator ->
      [
        "Your health became your time and your time became your health???";
        "The Time entity wasn't very tasty...";
      ]

let flavor_to_string (item : item) =
  let _, _, _, f, _ = item in
  f

let rarity_to_string (item : item) =
  let _, r, _, _, _ = item in
  match r with
  | Common -> "Common"
  | Rare -> "Rare"
  | Epic -> "Epic"
  | Undiscovered -> "Undiscovered"
