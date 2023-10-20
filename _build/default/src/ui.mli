module type UiType = sig
  type t

  val start : unit -> string -> unit
  val draw_timer : unit -> unit
end

module UiControl : UiType