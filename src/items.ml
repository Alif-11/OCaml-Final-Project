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
  | BigFryingPan
  | WordEviscerInator
  | BlackCatTrinket

type rarity =
  | Common
  | Rare
  | Epic
  | Undiscovered

type item =
  name * rarity * string * string * string list * (unit -> string -> unit)

let apple_effect () (mode : string) =
  if mode = "Easy" then
    EasyGameMutable._health :=
      min (EasyGameMutable.health () + 5) (EasyGameMutable.max_health ())
  else if mode = "Normal" then
    NormalGameMutable._health :=
      min (NormalGameMutable.health () + 5) (NormalGameMutable.max_health ())
  else if mode = "Hard" then
    HardGameMutable._health :=
      min (HardGameMutable.health () + 5) (HardGameMutable.max_health ())
  else if mode = "Extreme" then
    ExtremeGameMutable._health :=
      min (ExtremeGameMutable.health () + 5) (ExtremeGameMutable.max_health ())
  else if mode = "Sudden Death" then
    SuddenDeathMutable._health :=
      min (SuddenDeathMutable.health () + 5) (SuddenDeathMutable.max_health ())
  else if mode = "Chaos" then
    ChaosGameMutable._health :=
      min (ChaosGameMutable.health () + 5) (ChaosGameMutable.max_health ())
  else failwith "Item cringe"

let apple : item =
  (Apple, Common, "Banapple", "Tastes like a banana.", [ "+5 HP" ], apple_effect)

let banana_effect () (mode : string) =
  if mode = "Easy" then
    EasyGameMutable._health :=
      min (EasyGameMutable.health () + 5) (EasyGameMutable.max_health ())
  else if mode = "Normal" then
    NormalGameMutable._health :=
      min (NormalGameMutable.health () + 5) (NormalGameMutable.max_health ())
  else if mode = "Hard" then
    HardGameMutable._health :=
      min (HardGameMutable.health () + 5) (HardGameMutable.max_health ())
  else if mode = "Extreme" then
    ExtremeGameMutable._health :=
      min (ExtremeGameMutable.health () + 5) (ExtremeGameMutable.max_health ())
  else if mode = "Sudden Death" then
    SuddenDeathMutable._health :=
      min (SuddenDeathMutable.health () + 5) (SuddenDeathMutable.max_health ())
  else if mode = "Chaos" then
    ChaosGameMutable._health :=
      min (ChaosGameMutable.health () + 5) (ChaosGameMutable.max_health ())
  else failwith "Item cringe"

let banana : item =
  ( Banana,
    Common,
    "Appanana",
    "Tastes like an apple.",
    [ "+5 HP" ],
    banana_effect )

let broken_clock_effect () (mode : string) =
  if mode = "Easy" then EasyGameMutable._time := EasyGameMutable.time () + 10
  else if mode = "Normal" then
    NormalGameMutable._time := NormalGameMutable.time () + 10
  else if mode = "Hard" then
    HardGameMutable._time := HardGameMutable.time () + 10
  else if mode = "Extreme" then
    ExtremeGameMutable._time := ExtremeGameMutable.time () + 10
  else if mode = "Sudden Death" then
    SuddenDeathMutable._time := SuddenDeathMutable.time () + 10
  else if mode = "Chaos" then
    ChaosGameMutable._time := ChaosGameMutable.time () + 10
  else failwith "Item cringe"

let broken_clock : item =
  ( BrokenClock,
    Rare,
    "A Broken Clock",
    "It's right twice a day, even though it's ten seconds off.",
    [ "+10 Seconds" ],
    broken_clock_effect )

let edible_clock_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._time := EasyGameMutable.time () + 10;
    EasyGameMutable._health :=
      min (EasyGameMutable.health () + 15) (EasyGameMutable.max_health ()))
  else if mode = "Normal" then (
    NormalGameMutable._time := NormalGameMutable.time () + 10;
    NormalGameMutable._health :=
      min (NormalGameMutable.health () + 15) (NormalGameMutable.max_health ()))
  else if mode = "Hard" then (
    HardGameMutable._time := HardGameMutable.time () + 10;
    HardGameMutable._health :=
      min (HardGameMutable.health () + 15) (HardGameMutable.max_health ()))
  else if mode = "Extreme" then (
    ExtremeGameMutable._time := ExtremeGameMutable.time () + 10;
    ExtremeGameMutable._health :=
      min (ExtremeGameMutable.health () + 15) (ExtremeGameMutable.max_health ()))
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._time := SuddenDeathMutable.time () + 10;
    SuddenDeathMutable._health :=
      min (SuddenDeathMutable.health () + 15) (SuddenDeathMutable.max_health ()))
  else if mode = "Chaos" then (
    ChaosGameMutable._time := ChaosGameMutable.time () + 10;
    ChaosGameMutable._health :=
      min (ChaosGameMutable.health () + 15) (ChaosGameMutable.max_health ()))
  else failwith "Item cringe"

let edible_clock : item =
  ( EdibleClock,
    Epic,
    "Let Them Eat Clock",
    "It ... kinda looks like cake??",
    [ "+10 HP"; "+15 Seconds" ],
    edible_clock_effect )

let chaos_effect () (mode : string) =
  if mode = "Easy" then
    if Random.int 2 = 0 then (
      EasyGameMutable._health := 1;
      EasyGameMutable._time := 60)
    else (
      EasyGameMutable._health := EasyGameMutable.max_health ();
      EasyGameMutable._time := 90)
  else if mode = "Normal" then
    if Random.int 2 = 0 then (
      NormalGameMutable._health := 1;
      NormalGameMutable._time := 60)
    else (
      NormalGameMutable._health := NormalGameMutable.max_health ();
      NormalGameMutable._time := 90)
  else if mode = "Hard" then
    if Random.int 2 = 0 then (
      HardGameMutable._health := 1;
      HardGameMutable._time := 60)
    else (
      HardGameMutable._health := HardGameMutable.max_health ();
      HardGameMutable._time := 90)
  else if mode = "Extreme" then
    if Random.int 2 = 0 then (
      ExtremeGameMutable._health := 1;
      ExtremeGameMutable._time := 60)
    else (
      ExtremeGameMutable._health := ExtremeGameMutable.max_health ();
      ExtremeGameMutable._time := 90)
  else if mode = "Sudden Death" then
    if Random.int 2 = 0 then (
      SuddenDeathMutable._health := 1;
      SuddenDeathMutable._time := 60)
    else (
      SuddenDeathMutable._health := SuddenDeathMutable.max_health ();
      SuddenDeathMutable._time := 90)
  else if mode = "Chaos" then
    if Random.int 2 = 0 then (
      ChaosGameMutable._health := 1;
      ChaosGameMutable._time := 60)
    else (
      ChaosGameMutable._health := ChaosGameMutable.max_health ();
      ChaosGameMutable._time := 90)
  else failwith "Item cringe"

let chaos : item =
  (Chaos, Undiscovered, "???", "Don't.", [ "???" ], chaos_effect)

let forgotton_altar_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._time := EasyGameMutable.time () / 2;
    EasyGameMutable._health :=
      min (EasyGameMutable.health () * 2) (EasyGameMutable.max_health ()))
  else if mode = "Normal" then (
    NormalGameMutable._time := NormalGameMutable.time () / 2;
    NormalGameMutable._health :=
      min (NormalGameMutable.health () * 2) (NormalGameMutable.max_health ()))
  else if mode = "Hard" then (
    HardGameMutable._time := HardGameMutable.time () / 2;
    HardGameMutable._health :=
      min (HardGameMutable.health () * 2) (HardGameMutable.max_health ()))
  else if mode = "Extreme" then (
    ExtremeGameMutable._time := ExtremeGameMutable.time () / 2;
    ExtremeGameMutable._health :=
      min (ExtremeGameMutable.health () * 2) (ExtremeGameMutable.max_health ()))
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._time := SuddenDeathMutable.time () / 2;
    SuddenDeathMutable._health :=
      min (SuddenDeathMutable.health () * 2) (SuddenDeathMutable.max_health ()))
  else if mode = "Chaos" then (
    ChaosGameMutable._time := ChaosGameMutable.time () / 2;
    ChaosGameMutable._health :=
      min (ChaosGameMutable.health () * 2) (ChaosGameMutable.max_health ()))
  else failwith "Item cringe"

let forgotton_altar : item =
  ( ForgottenAltar,
    Rare,
    "The TIME Altar",
    "This altar is showing clear wear and tear due to the passage of time. It \
     seems to want something from you...",
    [ "+? HP"; "-? Seconds" ],
    forgotton_altar_effect )

let bloody_altar_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._time := EasyGameMutable.time () * 2;
    EasyGameMutable._health :=
      max
        (min (EasyGameMutable.health () / 2) (EasyGameMutable.max_health ()))
        1)
  else if mode = "Normal" then (
    NormalGameMutable._time := NormalGameMutable.time () * 2;
    NormalGameMutable._health :=
      max
        (min
           (NormalGameMutable.health () / 2)
           (NormalGameMutable.max_health ()))
        1)
  else if mode = "Hard" then (
    HardGameMutable._time := HardGameMutable.time () * 2;
    HardGameMutable._health :=
      max
        (min (HardGameMutable.health () / 2) (HardGameMutable.max_health ()))
        1)
  else if mode = "Extreme" then (
    ExtremeGameMutable._time := ExtremeGameMutable.time () * 2;
    ExtremeGameMutable._health :=
      max
        (min
           (ExtremeGameMutable.health () / 2)
           (ExtremeGameMutable.max_health ()))
        1)
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._time := SuddenDeathMutable.time () * 2;
    SuddenDeathMutable._health :=
      max
        (min
           (SuddenDeathMutable.health () / 2)
           (SuddenDeathMutable.max_health ()))
        1)
  else if mode = "Chaos" then (
    ChaosGameMutable._time := ChaosGameMutable.time () * 2;
    ChaosGameMutable._health :=
      max
        (min (ChaosGameMutable.health () / 2) (ChaosGameMutable.max_health ()))
        1)
  else failwith "Item cringe"

let bloody_altar : item =
  ( BloodyAltar,
    Rare,
    "The BLOOD Altar",
    "This altar is overflowing with blood, but its pulsing for more. It seems \
     to want something from you...",
    [ "-? HP"; "+? Seconds" ],
    bloody_altar_effect )

let obfuscinator_effect () (mode : string) =
  if mode = "Easy" then (
    let tmp = EasyGameMutable.time () in
    EasyGameMutable._time := EasyGameMutable.health ();
    EasyGameMutable._health := tmp)
  else if mode = "Normal" then (
    let tmp = NormalGameMutable.time () in
    NormalGameMutable._time := NormalGameMutable.health ();
    NormalGameMutable._health := tmp)
  else if mode = "Hard" then (
    let tmp = HardGameMutable.time () in
    HardGameMutable._time := HardGameMutable.health ();
    HardGameMutable._health := tmp)
  else if mode = "Extreme" then (
    let tmp = ExtremeGameMutable.time () in
    ExtremeGameMutable._time := ExtremeGameMutable.health ();
    ExtremeGameMutable._health := tmp)
  else if mode = "Sudden Death" then (
    let tmp = SuddenDeathMutable.time () in
    SuddenDeathMutable._time := SuddenDeathMutable.health ();
    SuddenDeathMutable._health := tmp)
  else if mode = "Chaos" then (
    let tmp = ChaosGameMutable.time () in
    ChaosGameMutable._time := ChaosGameMutable.health ();
    ChaosGameMutable._health := tmp)
  else failwith "Item cringe"

let obfuscinator : item =
  ( Obfuscinator,
    Epic,
    "The Obfuscinator!!!",
    "Doofenshmirtz's latest invention! Allows you to make a deal with Time in \
     exchange for your soul.",
    [ "HP <==> Seconds" ],
    obfuscinator_effect )

let jetpack_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._health := EasyGameMutable.health () - 10;
    EasyGameMutable.adjust_level ())
  else if mode = "Normal" then (
    NormalGameMutable._health := NormalGameMutable.health () - 10;
    NormalGameMutable.adjust_level ())
  else if mode = "Hard" then (
    HardGameMutable._health := HardGameMutable.health () - 10;
    HardGameMutable.adjust_level ())
  else if mode = "Extreme" then (
    ExtremeGameMutable._health := ExtremeGameMutable.health () - 10;
    ExtremeGameMutable.adjust_level ())
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._health := SuddenDeathMutable.health () - 10;
    SuddenDeathMutable.adjust_level ())
  else if mode = "Chaos" then (
    ChaosGameMutable._health := ChaosGameMutable.health () - 10;
    ChaosGameMutable.adjust_level ())
  else failwith "Item cringe"

let jetpack : item =
  ( Jetpack,
    Rare,
    "Joyride",
    "You don't have your joyriding license with you, you can't use this!",
    [ "-10 HP"; "+1 Level" ],
    jetpack_effect )

let reverse_jetpack_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._health := EasyGameMutable.health () - 10;
    EasyGameMutable.decrement_level ())
  else if mode = "Normal" then (
    NormalGameMutable._health := NormalGameMutable.health () - 10;
    NormalGameMutable.decrement_level ())
  else if mode = "Hard" then (
    HardGameMutable._health := HardGameMutable.health () - 10;
    HardGameMutable.decrement_level ())
  else if mode = "Extreme" then (
    ExtremeGameMutable._health := ExtremeGameMutable.health () - 10;
    ExtremeGameMutable.decrement_level ())
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._health := SuddenDeathMutable.health () - 10;
    SuddenDeathMutable.decrement_level ())
  else if mode = "Chaos" then (
    ChaosGameMutable._health := ChaosGameMutable.health () - 10;
    ChaosGameMutable.decrement_level ())
  else failwith "Item cringe"

let reverse_jetpack : item =
  ( ReverseJetpack,
    Epic,
    "Suspicious Joyride",
    "You don't have your joyriding license with you, but you can still use \
     this!",
    [ "-10 HP"; "+1 Level..." ],
    jetpack_effect )

let big_frying_pan_effect () (mode : string) =
  if mode = "Easy" then (
    EasyGameMutable._max_health := max (EasyGameMutable.max_health () - 5) 1;
    EasyGameMutable._health :=
      min (EasyGameMutable.max_health ()) (EasyGameMutable.health ()))
  else if mode = "Normal" then (
    NormalGameMutable._max_health := max (NormalGameMutable.max_health () - 5) 1;
    NormalGameMutable._health :=
      min (NormalGameMutable.max_health ()) (NormalGameMutable.health ()))
  else if mode = "Hard" then (
    HardGameMutable._max_health := max (HardGameMutable.max_health () - 5) 1;
    HardGameMutable._health :=
      min (HardGameMutable.max_health ()) (HardGameMutable.health ()))
  else if mode = "Extreme" then (
    ExtremeGameMutable._max_health :=
      max (ExtremeGameMutable.max_health () - 5) 1;
    ExtremeGameMutable._health :=
      min (ExtremeGameMutable.max_health ()) (ExtremeGameMutable.health ()))
  else if mode = "Sudden Death" then (
    SuddenDeathMutable._max_health :=
      max (SuddenDeathMutable.max_health () - 5) 1;
    SuddenDeathMutable._health :=
      min (SuddenDeathMutable.max_health ()) (SuddenDeathMutable.health ()))
  else if mode = "Chaos" then (
    ChaosGameMutable._max_health := max (ChaosGameMutable.max_health () - 5) 1;
    ChaosGameMutable._health :=
      min (ChaosGameMutable.max_health ()) (ChaosGameMutable.health ()))
  else failwith "Item cringe"

let big_frying_pan : item =
  ( BigFryingPan,
    Rare,
    "Beeg Frying Pan",
    "It's ... it's big! And it looks angry!",
    [ "+5 Max HP (?)" ],
    big_frying_pan_effect )

let word_eviscer_inator_effect () (mode : string) =
  if mode = "Easy" then
    EasyGameMutable._num_words := max (EasyGameMutable.num_words () - 4) 10
  else if mode = "Normal" then
    NormalGameMutable._num_words := max (NormalGameMutable.num_words () - 4) 10
  else if mode = "Hard" then
    HardGameMutable._num_words := max (HardGameMutable.num_words () - 4) 10
  else if mode = "Extreme" then
    ExtremeGameMutable._num_words :=
      max (ExtremeGameMutable.num_words () - 4) 10
  else if mode = "Sudden Death" then
    SuddenDeathMutable._num_words :=
      max (SuddenDeathMutable.num_words () - 4) 10
  else if mode = "Chaos" then
    ChaosGameMutable._num_words := max (ChaosGameMutable.num_words () - 4) 10
  else failwith "Item cringe"

let word_eviscer_inator : item =
  ( WordEviscerInator,
    Epic,
    "Hungry Hungry Word Hippo",
    "It's hungry for knowledge, and words are the easiest way to that!",
    [ "-4 Number of Words" ],
    word_eviscer_inator_effect )

let black_cat_trinket_effect () (mode : string) =
  if mode = "Easy" then
    EasyGameMutable._health :=
      max 1 (EasyGameMutable.max_health () - EasyGameMutable.health ())
  else if mode = "Normal" then
    NormalGameMutable._health :=
      max 1 (NormalGameMutable.max_health () - NormalGameMutable.health ())
  else if mode = "Hard" then
    HardGameMutable._health :=
      max 1 (HardGameMutable.max_health () - HardGameMutable.health ())
  else if mode = "Extreme" then
    ExtremeGameMutable._health :=
      max 1 (ExtremeGameMutable.max_health () - ExtremeGameMutable.health ())
  else if mode = "Sudden Death" then
    SuddenDeathMutable._health :=
      max 1 (SuddenDeathMutable.max_health () - SuddenDeathMutable.health ())
  else if mode = "Chaos" then
    ChaosGameMutable._health :=
      max 1 (ChaosGameMutable.max_health () - ChaosGameMutable.health ())
  else failwith "Item cringe"

let black_cat_trinket : item =
  ( BlackCatTrinket,
    Undiscovered,
    "A Cat Trinket",
    "An unfortunate encounter.",
    [ "??? h*a*th" ],
    black_cat_trinket_effect )

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
      rare =
        [|
          broken_clock; forgotton_altar; bloody_altar; jetpack; big_frying_pan;
        |];
      epic =
        [| edible_clock; obfuscinator; reverse_jetpack; word_eviscer_inator |];
      undiscovered = [| chaos; black_cat_trinket |];
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
  | Chaos -> [ "You were warned..." ]
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
  | BigFryingPan -> [ "Haha! You lost 5 max health! Hahah!!" ]
  | WordEviscerInator ->
      [
        "The hippo thanks you for the feast.";
        "You have 4 less words to deal with in the next round.";
      ]
  | BlackCatTrinket -> [ "You were warned..." ]

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

let apply_item (item : item) (mode : string) =
  let _, _, _, _, _, f = item in
  f () mode
