open State

(* Signature detailing basic properties of any item. *)
module type Items = sig
  (* How likely said item is to be dropped. Its value can range from 1 (1% drop
     rate) to 100 (100% drop rate). *)
  val drop_chance : int
  val health_change : int
end
