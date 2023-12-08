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
  | Jetpack
  | ReverseJetpack

type rarity =
  | Common
  | Rare
  | Epic
  | Undiscovered

type item = name * rarity * string * string * string list * (unit -> unit)

let apple_effect () =
  NormalGameMutable._health :=
    min (NormalGameMutable.health () + 5) (NormalGameMutable.max_health ())

let apple : item =
  (Apple, Common, "Banapple", "Tastes like a banana.", [ "+5 HP" ], apple_effect)

let banana_effect () =
  NormalGameMutable._health :=
    min (NormalGameMutable.health () + 5) (NormalGameMutable.max_health ())

let banana : item =
  ( Banana,
    Common,
    "Appanana",
    "Tastes like an apple.",
    [ "+5 HP" ],
    banana_effect )

let broken_clock_effect () =
  NormalGameMutable._time := NormalGameMutable.time () + 10

let broken_clock : item =
  ( BrokenClock,
    Rare,
    "A Broken Clock",
    "It's right twice a day, even though it's ten seconds off.",
    [ "+10 Seconds" ],
    broken_clock_effect )

let edible_clock_effect () =
  NormalGameMutable._health :=
    min (NormalGameMutable.health () + 10) (NormalGameMutable.max_health ());
  NormalGameMutable._time := NormalGameMutable.time () + 15

let edible_clock : item =
  ( EdibleClock,
    Epic,
    "Let Them Eat Clock",
    "It ... kinda looks like cake??",
    [ "+10 HP"; "+15 Seconds" ],
    edible_clock_effect )

let chaos_effect () =
  if Random.int 2 = 0 then (
    NormalGameMutable._health := 1;
    NormalGameMutable._time := 60)
  else (
    NormalGameMutable._health := 250;
    NormalGameMutable._time := 90)

let chaos : item =
  (Chaos, Undiscovered, "???", "Don't.", [ "???" ], chaos_effect)

let forgotton_altar_effect () =
  NormalGameMutable._time := NormalGameMutable.time () / 2;
  NormalGameMutable._health :=
    min (NormalGameMutable.health () * 2) (NormalGameMutable.max_health ())

let forgotton_altar : item =
  ( ForgottenAltar,
    Rare,
    "The TIME Altar",
    "This altar is showing clear wear and tear due to the passage of time. It \
     seems to want something from you...",
    [ "+? HP"; "-? Seconds" ],
    forgotton_altar_effect )

let bloody_altar_effect () =
  NormalGameMutable._time := NormalGameMutable.time () * 2;
  NormalGameMutable._health := max (min (NormalGameMutable.health () / 2) 100) 1

let bloody_altar : item =
  ( BloodyAltar,
    Rare,
    "The BLOOD Altar",
    "This altar is overflowing with blood, but its pulsing for more. It seems \
     to want something from you...",
    [ "-? HP"; "+? Seconds" ],
    forgotton_altar_effect )

let obfuscinator_effect () =
  let tmp = NormalGameMutable.time () in
  NormalGameMutable._time := NormalGameMutable.health ();
  NormalGameMutable._health := tmp

let obfuscinator : item =
  ( Obfuscinator,
    Epic,
    "The Obfuscinator!!!",
    "Doofenshmirtz's latest invention! Allows you to make a deal with Time in \
     exchange for your soul.",
    [ "HP <==> Seconds" ],
    obfuscinator_effect )

let jetpack_effect () =
  NormalGameMutable._health := NormalGameMutable.health () - 10;
  NormalGameMutable.adjust_level ()

let jetpack : item =
  ( Jetpack,
    Rare,
    "Joyride",
    "You don't have your joyriding license with you, you can't use this!",
    [ "-10 HP"; "+1 Level" ],
    jetpack_effect )

let reverse_jetpack_effect () =
  NormalGameMutable._health := NormalGameMutable.health () - 10;
  NormalGameMutable.decrement_level ()

let reverse_jetpack : item =
  ( ReverseJetpack,
    Epic,
    "Suspicious Joyride",
    "You don't have your joyriding license with you, but you can still use \
     this!",
    [ "-10 HP"; "+1 Level..." ],
    jetpack_effect )

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
      rare = [| broken_clock; forgotton_altar; bloody_altar; jetpack |];
      epic = [| edible_clock; obfuscinator; reverse_jetpack |];
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
  let _, _, n, _, _, _ = item in
  n

let effect_to_string (item : item) =
  let i, _, _, _, _, _ = item in
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
  | Jetpack ->
      [
        "You crashed through the roof, losing 10 heatlth.";
        "But, you skipped a level!!!";
      ]
  | ReverseJetpack ->
      [
        "You put the jetpack on upside down, losing 10 heatlth.";
        "You also went down a level!!!";
      ]

let flavor_to_string (item : item) =
  let _, _, _, f, _, _ = item in
  f

let rarity_to_string (item : item) =
  let _, r, _, _, _, _ = item in
  match r with
  | Common -> "Common"
  | Rare -> "Rare"
  | Epic -> "Epic"
  | Undiscovered -> "Undiscovered"

let stats_to_string (item : item) =
  let _, _, _, _, s, _ = item in
  s

let apply_item (item : item) =
  let _, _, _, _, _, f = item in
  f ()
