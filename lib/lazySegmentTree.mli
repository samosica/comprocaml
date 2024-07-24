module type S = sig
  type elt
  type act
  type t

  val make : int -> t
  val set : int -> elt -> t -> unit
  val apply_range : int -> int -> act -> t -> unit
  val product : int -> int -> t -> elt
  val binary_search : (elt -> bool) -> t -> int
end

module Make
  (M : Monoid.S)
  (A : Monoid.Action with type t = M.t)
: S with type elt = M.t and type act = A.M.t
